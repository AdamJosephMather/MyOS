#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include "font8x8_basic.h"

extern "C" {
	#include "../limine.h"
}

__attribute__((used, section(".limine_requests")))
static volatile LIMINE_BASE_REVISION(4);

__attribute__((used, section(".limine_requests")))
static volatile struct limine_framebuffer_request fb_req = {
	.id = LIMINE_FRAMEBUFFER_REQUEST,
	.revision = 0
};

__attribute__((used, section(".limine_requests")))
static volatile struct limine_rsdp_request rsdp_req = {
	.id = LIMINE_RSDP_REQUEST,
	.revision = 0
};

__attribute__((used, section(".limine_requests")))
static volatile struct limine_memmap_request memmap_req = {
	.id = LIMINE_MEMMAP_REQUEST,
	.revision = 0
};

__attribute__((used, section(".limine_requests")))
static volatile struct limine_hhdm_request hhdm_req = {
	.id = LIMINE_HHDM_REQUEST,
	.revision = 0
};

__attribute__((used, section(".limine_requests_start")))
static volatile LIMINE_REQUESTS_START_MARKER;
__attribute__((used, section(".limine_requests_end")))
static volatile LIMINE_REQUESTS_END_MARKER;


struct limine_framebuffer *fb;
uint64_t HHDM;
bool debug = false;


static const char map_unshift[0x60] = {
/*00*/0,  27, '1','2','3','4','5','6','7','8','9','0','-','=', 8,  9,
/*10*/'q','w','e','r','t','y','u','i','o','p','[',']', 13, 0,  'a','s',
/*20*/'d','f','g','h','j','k','l',';', '\'', '`', 0, '\\','z','x','c','v',
/*30*/'b','n','m',',','.','/', 0,  '*', 0,  ' ', 0,   0,   0,   0,   0,  0,
/*40*/0,  0,  0,  0,  0,  0,  0,  '7','8','9','-', '4','5','6','+', '1',
/*50*/'2','3','0','.', 0,  0,  0,  0,  0,  0
};
static const char map_shift[0x60] = {
/*00*/0,  27, '!','@','#','$','%','^','&','*','(',')','_','+', 8,  9,
/*10*/'Q','W','E','R','T','Y','U','I','O','P','{','}', 13, 0,  'A','S',
/*20*/'D','F','G','H','J','K','L',':','"', '~', 0,  '|','Z','X','C','V',
/*30*/'B','N','M','<','>','?', 0,  '*', 0,  ' ', 0,   0,   0,   0,   0,  0,
/*40*/0,  0,  0,  0,  0,  0,  0,  '7','8','9','-', '4','5','6','+', '1',
/*50*/'2','3','0','.', 0,  0,  0,  0,  0,  0
};

static inline void hcf() {
	for (;;) { __asm__ __volatile__("hlt"); }
}

static inline void putp(uint32_t x, uint32_t y, uint32_t argb) {
	if (x >= fb->width || y >= fb->height) return;
	volatile uint32_t *base = (volatile uint32_t *)fb->address;
	base[y * (fb->pitch / 4) + x] = argb;
}

static uint32_t toARGB(uint8_t a, uint8_t r, uint8_t g, uint8_t b) {
	uint32_t out = a;
	out = out << 8;
	out |= r;
	out = out << 8;
	out |= g;
	out = out << 8;
	out |= b;
	return out;
}

static inline uint8_t inb(uint16_t port) {
	uint8_t ret;
	__asm__ volatile ("inb %1, %0" : "=a"(ret) : "Nd"(port));
	return ret;
}

static bool canGetKey() {
	return inb(0x64) & 1;
}

static uint8_t readScancode() {
	return inb(0x60);
}

struct KeyState { bool lshift=false, rshift=false, caps=false; };
static inline bool is_make(uint8_t sc) { return (sc & 0x80) == 0; }
static inline uint8_t code(uint8_t sc) { return sc & 0x7F; }

static char scancode_to_char(uint8_t sc, KeyState& ks) {
	// shift keys
	if (is_make(sc)) {
		if (code(sc)==0x2A) ks.lshift = true;    // LShift
		if (code(sc)==0x36) ks.rshift = true;    // RShift
		if (code(sc)==0x3A) ks.caps   = !ks.caps; // Caps toggles on make
	} else { // break
		if (code(sc)==0x2A) ks.lshift = false;
		if (code(sc)==0x36) ks.rshift = false;
	}
	
	if (!is_make(sc)) return 0; // ignore key releases
	
	uint8_t idx = code(sc);
	if (idx >= 0x60) return 0;
	
	bool shift = ks.lshift || ks.rshift;
	char c = shift ? map_shift[idx] : map_unshift[idx];
	
	// Apply caps to letters only
	if (ks.caps && c >= 'A' && c <= 'Z') c = (char)(c + 32);
	else if (ks.caps && c >= 'a' && c <= 'z') c = (char)(c - 32);
	
	return c;
}

static inline void fill_screen_fast(uint32_t argb) {
	volatile uint8_t* base = (uint8_t*)fb->address;
	const uint32_t w = fb->width;
	for (uint32_t y = 0; y < fb->height; ++y) {
		volatile uint32_t* row = (uint32_t*)(base + y * fb->pitch);
		for (uint32_t x = 0; x < w; ++x) {
			row[x] = argb;
		}
	}
}






static inline uint32_t inl(uint16_t port) {
	uint32_t ret;
	__asm__ volatile ("inl %1, %0" : "=a"(ret) : "Nd"(port));
	return ret;
}

static inline void outl(uint16_t port, uint32_t val) {
	__asm__ volatile ("outl %0, %1" : : "a"(val), "Nd"(port));
}








#define PCI_ECAM_VIRT_BASE 0xFFFF900000000000ULL

struct ACPISDTHeader {
	char     signature[4];
	uint32_t length;
	uint8_t  revision;
	uint8_t  checksum;
	char     oem_id[6];
	char     oem_table_id[8];
	uint32_t oem_revision;
	uint32_t creator_id;
	uint32_t creator_revision;
} __attribute__((packed));

struct MCFGEntry {
	uint64_t base_address;
	uint16_t pci_segment_group;
	uint8_t  start_bus;
	uint8_t  end_bus;
	uint32_t reserved;
} __attribute__((packed));

struct MCFGHeader {
	struct ACPISDTHeader header;
	uint64_t reserved;           // <-- required padding
	struct MCFGEntry entries[];
} __attribute__((packed));

// returns a DWORD pointer in ECAM
inline volatile uint32_t* pci_cfg_ptr32(uint64_t virt_base, uint8_t start_bus, uint8_t bus, uint8_t dev, uint8_t fn, uint16_t off) {
	// ECAM: 1MiB per bus, 32 devs, 8 funcs, 4KiB per fn
	// Validate offset is DWORD-aligned and within 4KiB
	// (you can drop these checks in release):
	if ((off & 3) || off > 0xFFC) hcf();

	uint64_t addr = virt_base
				  + ((uint64_t)(bus - start_bus) << 20)  // (bus - start)*1MiB
				  + ((uint64_t)dev << 15)                // dev*32*4KiB
				  + ((uint64_t)fn  << 12)                // fn*4KiB
				  + off;

	return (volatile uint32_t*)addr;  // DO NOT add PCI_ECAM_VIRT_BASE again
}

inline volatile uint16_t* pci_cfg_ptr16(uint64_t virt_base, uint8_t start_bus, uint8_t bus, uint8_t dev, uint8_t fn, uint16_t off) {
	if ((off & 1) || off > 0xFFE) hcf();
	uint64_t addr = virt_base
				  + ((uint64_t)(bus - start_bus) << 20)
				  + ((uint64_t)dev << 15)
				  + ((uint64_t)fn  << 12)
				  + off;
	return (volatile uint16_t*)addr;
}


uint32_t pci_cfg_read32(uint64_t virt_base, uint8_t start_bus, uint8_t bus, uint8_t dev, uint8_t fn, uint16_t off) {
	volatile uint32_t* val = pci_cfg_ptr32(virt_base, start_bus, bus, dev, fn, off);
	return *val;
}

void pci_cfg_write32(uint64_t virt_base, uint8_t start_bus, uint8_t bus, uint8_t dev, uint8_t fn, uint16_t off, uint32_t towrite) {
	volatile uint32_t* val = pci_cfg_ptr32(virt_base, start_bus, bus, dev, fn, off);
	*val = towrite;
}

uint16_t pci_cfg_read16(uint64_t virt_base, uint8_t start_bus, uint8_t bus, uint8_t dev, uint8_t fn, uint16_t off) {
	volatile uint16_t* val = pci_cfg_ptr16(virt_base, start_bus, bus, dev, fn, off);
	return *val;
}

void pci_cfg_write16(uint64_t virt_base, uint8_t start_bus, uint8_t bus, uint8_t dev, uint8_t fn, uint16_t off, uint16_t towrite) {
	volatile uint16_t* val = pci_cfg_ptr16(virt_base, start_bus, bus, dev, fn, off);
	*val = towrite;
}

void u64_to_str(uint64_t value, char* buffer) {
	if (value == 0) {
		buffer[0] = '0';
		buffer[1] = '\0';
		return;
	}
	
	int i = 0;
	while (value > 0) {
		buffer[i] = '0'+(value%10);
		value /= 10;
		i++;
	}
	buffer[i] = '\0';
	
	for (int j = 0; j < i/2; j++) {
		auto temp = buffer[j];
		buffer[j] = buffer[i-j-1];
		buffer[i-j-1] = temp;
	}
}

char HEX_NUMS[] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};

void u64_to_hex(uint64_t value, char* buffer) {
	int n = 18; 
	buffer[n--] = '\0';
	
	for (int i = 0; i < 16; ++i) {
		uint8_t digit = value & 0xF;
		buffer[n--] = HEX_NUMS[digit];
		value >>= 4;
	}
	buffer[n--] = 'x';
	buffer[n--] = '0';
}

void draw_char(int x, int y, char c, uint32_t color) {
	const uint8_t* glyph = font8x8_basic[(int)c];
	for (int row = 0; row < 8; row++) {
		uint8_t bits = glyph[row];
		for (int col = 0; col < 8; col++) {
			if (bits & (1 << col))
				putp(x + col, y + row, color);
		}
	}
}

int current_print_line = 0;

void print(char* buffer) {
	uint64_t i = 0;
	while (true) {
		char c = buffer[i];
		if (c == '\0') {
			break;
		}
		draw_char(i*8, current_print_line*10, c, 0xFFFFFFFF);
		i++;
	}
	
	current_print_line ++;
}












#define PAGE_SIZE 0x1000

static inline uint64_t read_cr3() {
	uint64_t value;
	asm volatile ("mov %%cr3, %0" : "=r"(value));
	return value;
}

static uint64_t next_free_phys_page = 0;
static uint64_t max_free_phys_page = 0;
const uint64_t GUARD = 2*1024*1024; // 2 MiB guard
static uint64_t max_hhdm_size = 0;

void init_physical_allocator() {
	auto memmap = memmap_req.response;
	uint64_t best_len = 0;
	uint64_t best_base = 0;

	for (uint64_t i = 0; i < memmap->entry_count; i++) {
		auto *entry = memmap->entries[i];
		if (entry->type == LIMINE_MEMMAP_USABLE && entry->length > best_len && entry->base > GUARD) {
			best_len = entry->length;
			best_base = (entry->base + PAGE_SIZE - 1) & ~(PAGE_SIZE - 1);
		}
		if (entry->length + entry->base > max_hhdm_size) {
			max_hhdm_size = entry->length + entry->base;
		}
	}

	next_free_phys_page = best_base;
	max_free_phys_page = best_base+best_len;
	
//	if (debug) {
	char str[64];
	
	u64_to_hex(next_free_phys_page, str);
	print("Next free phys page");
	print(str);
	
	u64_to_hex(max_free_phys_page, str);
	print("Max free phys page");
	print(str);
//	}
}

uint64_t alloc_phys_page() {
	if (next_free_phys_page >= max_free_phys_page) {
		print("OUT OF MEMORY!");
		hcf();
	}
	
	uint64_t addr = next_free_phys_page;
	next_free_phys_page += PAGE_SIZE;
	
	return addr;
}

#define PML4_INDEX(va) (((va) >> 39) & 0x1FF)
#define PDPT_INDEX(va) (((va) >> 30) & 0x1FF)
#define PD_INDEX(va)   (((va) >> 21) & 0x1FF)
#define PT_INDEX(va)   (((va) >> 12) & 0x1FF)

enum PageFlags : uint64_t {
	PRESENT  = 1ULL << 0,
	WRITABLE = 1ULL << 1,
	USER     = 1ULL << 2,
	WRITE_THROUGH = 1ULL << 3,
	CACHE_DISABLE = 1ULL << 4,
	ACCESSED = 1ULL << 5,
	DIRTY    = 1ULL << 6,
	HUGE     = 1ULL << 7,
	GLOBAL   = 1ULL << 8,
	NX       = 1ULL << 63
};

static inline void invlpg(uint64_t addr) {
	asm volatile("invlpg (%0)" :: "r"(addr) : "memory");
}

#define PTE_ADDR_MASK     0x000ffffffffff000ULL
#define PD_2M_ADDR_MASK   0x000ffffffe00000ULL
#define PDP_1G_ADDR_MASK  0x000fffffc0000000ULL

static inline volatile uint64_t *alloc_table(void) {
	uint64_t phys = alloc_phys_page();
	volatile uint64_t *virt = (volatile uint64_t *)(HHDM + phys);
	for (int i = 0; i < 512; i++) virt[i] = 0;
	return virt;
}

static inline volatile uint64_t *descend(volatile uint64_t *parent, size_t idx, int level /*3=PDPT,2=PD*/) {
	uint64_t e = parent[idx];

	// Not present -> allocate next-level table
	if (!(e & PRESENT)) {
		volatile uint64_t *child = alloc_table();
		uint64_t phys = (uint64_t)child - HHDM;
		parent[idx] = (phys & PTE_ADDR_MASK) | PRESENT | WRITABLE;
		return child;
	}

	// Split 1 GiB huge (PDPT->PD)
	if (level == 3 && (e & HUGE)) {
		uint64_t flags = e & ~(PTE_ADDR_MASK | HUGE);
		uint64_t base  = e & PDP_1G_ADDR_MASK;          // 1 GiB-aligned
		volatile uint64_t *pd = alloc_table();
		for (int i = 0; i < 512; i++)
			pd[i] = (base + ((uint64_t)i << 21)) | flags | PRESENT | HUGE;  // 2 MiB each
		uint64_t phys = (uint64_t)pd - HHDM;
		parent[idx] = (phys & PTE_ADDR_MASK) | (flags & ~HUGE) | PRESENT | WRITABLE;
		return pd;
	}

	// Split 2 MiB huge (PD->PT)
	if (level == 2 && (e & HUGE)) {
		uint64_t flags = e & ~(PTE_ADDR_MASK | HUGE);
		uint64_t base  = e & PD_2M_ADDR_MASK;           // 2 MiB-aligned
		volatile uint64_t *pt = alloc_table();
		for (int i = 0; i < 512; i++)
			pt[i] = (base + ((uint64_t)i << 12)) | flags | PRESENT;         // 4 KiB each
		uint64_t phys = (uint64_t)pt - HHDM;
		parent[idx] = (phys & PTE_ADDR_MASK) | (flags & ~HUGE) | PRESENT | WRITABLE;
		return pt;
	}

	// Already a pointer to the next level
	return (volatile uint64_t *)(HHDM + (e & PTE_ADDR_MASK));
}

void map_page(uint64_t va, uint64_t pa, uint64_t flags) {
	volatile uint64_t *pml4 = (volatile uint64_t *)(HHDM + (read_cr3() & PTE_ADDR_MASK));
	volatile uint64_t *pdpt = descend(pml4, PML4_INDEX(va), 4);  // top level is not huge
	volatile uint64_t *pd   = descend(pdpt, PDPT_INDEX(va), 3);  // split 1 GiB if needed
	volatile uint64_t *pt   = descend(pd,   PD_INDEX(va),   2);  // split 2 MiB if needed

	pt[PT_INDEX(va)] = (pa & PTE_ADDR_MASK) | (flags | PRESENT);
	asm volatile("invlpg (%0)" :: "r"(va) : "memory");
}

void map_ecam_region(uint64_t phys_base, uint64_t virt_base, uint64_t size) {
	for (uint64_t offset = 0; offset < size; offset += 0x1000) {
		map_page(virt_base + offset, phys_base + offset, WRITABLE | CACHE_DISABLE | WRITE_THROUGH);
	}
}

void map_mmio_region(uint64_t phys_base, uint64_t virt_base, uint64_t size) {
	for (uint64_t offset = 0; offset < size; offset += 0x1000) {
		map_page(virt_base + offset, phys_base + offset, WRITABLE | CACHE_DISABLE | WRITE_THROUGH);
	}
}

void map_region(uint64_t virt_start, uint64_t phys_start, size_t length, uint64_t flags) {
	size_t pages = (length + 0xFFF) / 0x1000;
	
	for (size_t i = 0; i < pages; i++) {
		map_page(virt_start + i * 0x1000, phys_start + i * 0x1000, flags);
	}
}

struct __attribute__((packed)) XHCICapRegs {
	uint8_t caplength;
	uint8_t reserved;
	uint16_t hciversion;
	uint32_t hcsparams1;
	uint32_t hcsparams2;
	uint32_t hcsparams3;
	uint32_t hccparams1;
	uint32_t dboff;
	uint32_t rtsoff;
};

uint64_t PCI_ECAM_VA_BASE = 0xFFFF'8000'0000'0000ULL; // pick a canonical kernel VA
uint64_t PCI_ECAM_SEG_STRIDE         = 0x10000000ULL; // 256 MiB

uint64_t PCI_MMIO_VA_BASE = 0xFFFF'9000'0000'0000ULL;  // canonical
uint64_t USB_VA_BASE = (PCI_MMIO_VA_BASE + 0x0010'0000ULL); // leave some gap


extern "C" void kmain(void) {
	// frame buffer
	if (!LIMINE_BASE_REVISION_SUPPORTED) hcf();
	if (!fb_req.response || fb_req.response->framebuffer_count < 1) hcf();
	
	fb = fb_req.response->framebuffers[0];
	
	if (!memmap_req.response) {
		print("No memmap response");
		hcf();
	}
	
	if (!hhdm_req.response) {
		print("No hhdm response");
		hcf();
	}
	
	HHDM = hhdm_req.response->offset;
	// HHDM + PA = VA
	
	
	// Let's go map the memmory
//	init_va_layout();
	init_physical_allocator();
	map_region(HHDM, 0, max_hhdm_size, PRESENT | WRITABLE);
	print("Mapped the entire ram.");
	
	// time to go find the pcie
	
	// RSDP table (will point to the thing needed to find PCIe)
	if (!rsdp_req.response) hcf();
	uint64_t rsdp_va = (uint64_t)rsdp_req.response->address;
	
	
	
	volatile uint8_t *ptr = (volatile uint8_t *)rsdp_va;
	
	// The expected signature
	const char expected[8] = {'R','S','D',' ','P','T','R',' '};
	
	// Compare byte-by-byte
	bool valid_signature = true;
	for (int i = 0; i < 8; i++) {
		if (ptr[i] != expected[i]) {
			valid_signature = false;
			break;
		}
	}
	
	if (!valid_signature) {
		print("No valid signature for rsd ptr");
		hcf();
	}
	
	// so that was the first 8 bytes of info.
	// Next there's 1 byte for checksum, 6 for oem, 1 for revision, 4 for rsdt_address
	
//	uint8_t revision = ptr[8+1+6+1 - 1];
	
	// Read the 32-bit RSDT address
	uint32_t rsdt_pa_32 =
		(uint32_t)ptr[0x10] |
		((uint32_t)ptr[0x11] << 8) |
		((uint32_t)ptr[0x12] << 16) |
		((uint32_t)ptr[0x13] << 24);
	
	
	uint64_t rsdt_va = HHDM+(uint64_t)rsdt_pa_32;
	volatile uint8_t *ptr_rsdt = (volatile uint8_t *)rsdt_va;
	
	// The expected signature
	const char expected_rsdt[4] = {'R','S','D','T'};
	
	// Compare byte-by-byte
	valid_signature = true;
	for (int i = 0; i < 4; i++) {
		if (ptr_rsdt[i] != expected_rsdt[i]) {
			valid_signature = false;
			break;
		}
	}
	
	if (!valid_signature) {
		print("No valid signature for rsdt");
		hcf();
	}
	
	uint32_t rsdt_length =
		(uint32_t)ptr_rsdt[4] |
		((uint32_t)ptr_rsdt[5] << 8) |
		((uint32_t)ptr_rsdt[6] << 16) |
		((uint32_t)ptr_rsdt[7] << 24);
	uint32_t entry_count = (rsdt_length - 36) / 4;
	
	// now that we have the rsdt, we need to skip 36 bytes, and that leads us to a list of 32 byte physical addresses. So, we'll itterate until we find 'MCFG' and that's our target (for pcie -> usb -> keyboard -> flexing on kai)
	
	volatile uint8_t *ptr_mcfg = 0;
	
	for (uint32_t i = 0; i < entry_count; i++) {
		uint32_t offset = 36 + i * 4;
	
		uint32_t entry_pa =
			(uint32_t)ptr_rsdt[offset] |
			((uint32_t)ptr_rsdt[offset+1] << 8) |
			((uint32_t)ptr_rsdt[offset+2] << 16) |
			((uint32_t)ptr_rsdt[offset+3] << 24);
	
		uint64_t entry_va = HHDM + (uint64_t)entry_pa;
		volatile uint8_t *entry = (volatile uint8_t *)entry_va;
		
		char str[5];
		str[0] = entry[0];
		str[1] = entry[1];
		str[2] = entry[2];
		str[3] = entry[3];
		str[4] = '\0';
	
//		print(str);
		if (str[0] == 'M' && str[1] == 'C' && str[2] == 'F' && str[3] == 'G') {
//			print("Found it!");
			ptr_mcfg = entry;
			break;
		}
	}
	
	if (ptr_mcfg == 0) {
		print("Ptr mcfg == 0");
		hcf();
	}
	
	
	volatile struct MCFGHeader *mcfg = (volatile struct MCFGHeader *)ptr_mcfg;
	
	uint32_t length = mcfg->header.length;
	uint32_t entries = (length - sizeof(struct ACPISDTHeader) - 8) / 16;  // 16 bytes per entry
	
	uint64_t usb_virt_base;
	uint8_t usb_start;
	uint8_t usb_bus;
	uint8_t usb_dev;
	uint8_t usb_fn;
	uint8_t usb_prog_if;
	bool usb_found = false;
	
	for (uint32_t i = 0; i < entries; i++) {
		auto *e = &mcfg->entries[i];
		
		uint64_t phys_base = e->base_address;
		uint16_t seg  = e->pci_segment_group;
		uint8_t start = e->start_bus;
		uint8_t end   = e->end_bus;
		
		print("nice - let's map this shit");
		
		uint64_t ecam_size = (uint64_t)(end - start + 1) * 0x100000ULL; // 1MiB per bus
		uint64_t virt_base = PCI_ECAM_VA_BASE + (uint64_t)seg * PCI_ECAM_SEG_STRIDE;
		// Map MMIO as UC:
		map_ecam_region(phys_base, virt_base, ecam_size);
		print("Mapped!");
		
		for (uint8_t bus = start; bus < end; bus++) {
			for (uint8_t dev = 0; dev < 32; dev++) {
				for (uint8_t fn = 0; fn < 8; fn++) {
					uint32_t id = pci_cfg_read32(virt_base, start, bus, dev, fn, 0x00);
					if (id == 0xFFFFFFFF) continue; // no device present
					
					uint32_t class_reg = pci_cfg_read32(virt_base, start, bus, dev, fn, 0x08);
					
					uint8_t baseclass = (class_reg >> 24) & 0xFF;
					uint8_t subclass  = (class_reg >> 16) & 0xFF;
					uint8_t prog_if   = (class_reg >> 8) & 0xFF;
					
					if (baseclass == 0x0C && subclass == 0x03) {
						// USB controller detected!
						usb_virt_base = virt_base;
						usb_start = start;
						usb_bus = bus;
						usb_dev = dev;
						usb_fn = fn;
						usb_prog_if = prog_if;
						usb_found = true;
						print("Found a USB controller!");
						
						break;
						// You can optionally differentiate:
						// if (prog_if == 0x30) -> XHCI
					}
				}
			}
		}
	}
	
	if (!usb_found) {
		print("Could not find a USB controller!");
		hcf();
	}
	
	
	char str[64];
	u64_to_hex(usb_prog_if, str);
	print("Prog_If:");
	print(str);
	
	if (usb_prog_if != 0x30) {
		print("Unsupported USB protocol.");
		hcf();
	}
	
	// now that we have the usb device here, we need to identify the location of the bar addresses.
	// essentially a bar is a register holding the loaction that will store the space in which we will communicate with the device.
	// usb uses 1, so does nvme ssd. However, network might use more than one, vga/gpu will use several. Right now let's focus only on usb
	
	uint32_t bar0_low  = pci_cfg_read32(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x10);
	uint32_t bar1_high = pci_cfg_read32(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x14);
	
	uint64_t bar_addr = ((uint64_t)bar1_high << 32) | (bar0_low & ~0xFULL);
	
	print("Got the bar addr...");
	u64_to_hex(bar_addr, str);
	print(str);
	
	
	
	uint16_t before = pci_cfg_read16(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x04);
	print("Before CMD: "); u64_to_hex(before, str); print(str);
	
	pci_cfg_write16(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x04, before | (1 << 1));
	uint16_t after = pci_cfg_read16(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x04);
	print("After CMD: "); u64_to_hex(after, str); print(str);
	
//	// this does... something
//	uint16_t cmd = pci_cfg_read16(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x04);
//	cmd |= (1 << 1); // enable MMIO
//	pci_cfg_write16(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x04, cmd);
	
	// Save old value
	pci_cfg_write32(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x10, 0xFFFFFFFF);
	uint32_t size_mask = pci_cfg_read32(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x10);
	pci_cfg_write32(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x10, bar0_low); // restore
	
	print("Got the size mask...");
	u64_to_hex(size_mask, str);
	print(str);
	
	uint64_t mmio_size = ~(size_mask & ~0xF) + 1;
	
	print("Got the mmio size...");
	u64_to_hex(mmio_size, str);
	print(str);
	
//	debug = true;
	map_mmio_region(bar_addr, USB_VA_BASE, mmio_size);
	
	print("Mapped USB");
	
	uint32_t info = *(volatile uint32_t*)USB_VA_BASE;
	u64_to_hex(info, str); print(str);
	
	u64_to_hex((info>>16)&0xFFFF, str); print(str);
	
	uint32_t cap_len = (info & 0xFF);
	uint32_t ver = (info>>16) & 0xFFFF;
	
	print("Cap len: ");
	u64_to_str(cap_len, str);
	print(str);
	
	print("XHCI found, version: ");
	u64_to_str(ver, str);
	print(str);
	
	print("Finished.");
	hcf();
}