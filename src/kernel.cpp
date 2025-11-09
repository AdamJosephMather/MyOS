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

void to_str(uint64_t value, char* buffer) {
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

void to_hex(uint64_t value, char* buffer) {
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
	
	to_hex(next_free_phys_page, str);
	print("Next free phys page");
	print(str);
	
	to_hex(max_free_phys_page, str);
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

uint64_t PCI_ECAM_VA_BASE = 0xFFFF'8000'0000'0000ULL; // pick a canonical kernel VA
uint64_t PCI_ECAM_SEG_STRIDE         = 0x10000000ULL; // 256 MiB

uint64_t PCI_MMIO_VA_BASE = 0xFFFF'9000'0000'0000ULL;  // canonical
uint64_t USB_VA_BASE = (PCI_MMIO_VA_BASE + 0x0010'0000ULL); // leave some gap


struct XHCIOpRegs {
	volatile uint32_t usbcmd;       // 00h
	volatile uint32_t usbsts;       // 04h
	volatile uint32_t pagesize;     // 08h (RO capabilities)
	volatile uint8_t  reserved1[8]; // 0Ch
	volatile uint32_t dnctrl;       // 14h
	volatile uint64_t crcr;         // 18h
	volatile uint8_t  reserved2[16];
	volatile uint64_t dcbaap;       // 30h
	volatile uint32_t config;       // 38h
};

struct TRB {
	uint64_t parameter;
	uint32_t status;
	uint32_t control;
} __attribute__((packed, aligned(16)));

struct ERSTEntry {
	uint64_t ring_segment_base;
	uint32_t ring_segment_size;
	uint32_t reserved;
} __attribute__((packed, aligned(16)));

struct Ring {
	volatile TRB *trb;   // 256 entries: 0..254 data, 255 = Link TRB
	uint64_t phys;
	uint32_t enq;        // 0..254
	uint8_t  pcs;        // Producer Cycle State (1 initially)
};

struct InputControlCtx {
	uint32_t drop_flags;    // which contexts to drop
	uint32_t add_flags;     // which contexts to add
	uint32_t rsvd[6];
};

struct SlotCtx {
	uint32_t route_string;
	uint32_t speed;      // set per-port speed
	uint32_t rsvd2;
	uint32_t rsvd3;
	uint32_t tt_hub_slotid;
	uint32_t tt_portnum;
	uint32_t port_num;   // which root port this device is on
	uint32_t rsvd4;
};

struct EndpointCtx {
	uint32_t ep_state;       // Dword 0
	uint32_t ep_flags;       // Dword 1
	uint64_t tr_deq_ptr;     // Dword 2-3
	uint32_t avg_trb_len;    // Dword 4
	uint32_t max_esit_hi;    // Dword 5
	uint32_t reserved[2];    // Dword 6-7
} __attribute__((packed));

void ring_init(struct Ring *r) {
	r->trb = (volatile TRB*)alloc_table();     // 4 KiB
	r->phys = (uint64_t)r->trb - HHDM;
	r->enq = 0;
	r->pcs = 1;

	// All TRBs must start with cycle=0
	for (int i = 0; i < 256; i++) r->trb[i].control = 0;

	// Link TRB at 255 -> back to start, Toggle Cycle
	r->trb[255].parameter = r->phys;
	r->trb[255].status = 0;
	r->trb[255].control = (6u<<10) | (1u<<1);  // Type=Link, TC=1, C=0
}

void ring_push_cmd(struct Ring *r, uint32_t ctrl, uint64_t param, uint32_t status) {
	volatile TRB *t = &r->trb[r->enq];
	t->parameter = param;
	t->status    = status;
	t->control   = (ctrl & ~1u) | r->pcs;  // set Cycle=PCS

	if (++r->enq == 255) {                 // wrap across Link TRB
		r->enq = 0;
		r->pcs ^= 1;
	}
}

// NEW: tiny spin delay; tuned big enough for "ms-ish" waits on boot
static inline void spin_delay(volatile uint64_t iters) {
	for (volatile uint64_t i=0; i<iters; ++i) { __asm__ __volatile__("pause"); }
}

// NEW: walk xECP and claim OS ownership; also kill legacy SMIs
static void xhci_legacy_handoff(uint32_t hcc1, uint64_t mmio_base) {
	uint32_t xecp_dw = (hcc1 >> 16) & 0xFFFF;   // dword offset from MMIO base
	while (xecp_dw) {
		volatile uint32_t *ec = (volatile uint32_t*)(mmio_base + (uint64_t)xecp_dw*4);
		uint32_t hdr   = ec[0];
		uint32_t capid =  hdr        & 0xFF;
		uint32_t next  = (hdr >> 8)  & 0xFF;

		if (capid == 1) { // USB Legacy Support
			volatile uint32_t *usblegsup    = ec + 0;   // xECP + 0x00
			volatile uint32_t *usblegctlsts = ec + 1;   // xECP + 0x04

			// Set OS Owned (bit 24)
			*usblegsup |= (1u << 24);
			// Wait BIOS Owned (bit 16) to clear
			for (int t=0; t<1000000 && (*usblegsup & (1u<<16)); ++t) { __asm__ __volatile__("pause"); }

			// Disable legacy SMIs and clear any pending RW1C
			*usblegctlsts = 0;           // clear enables (RW)
			*usblegctlsts = 0xFFFFFFFF;  // clear status (RW1C)
			break;
		}
		xecp_dw = next; // next is dword offset (0 means end)
	}
}

// NEW: Intel-only routing quirk (Panther Point & friends)
static void intel_route_all_ports(uint64_t virt_base,
								  uint8_t start, uint8_t bus, uint8_t dev, uint8_t fn)
{
	// Mirror masks the BIOS allowed into the live route registers.
	// XUSB2PRM -> XUSB2PR
	uint32_t xusb2prm = pci_cfg_read32(virt_base, start, bus, dev, fn, 0xD4);
	pci_cfg_write32(virt_base, start, bus, dev, fn, 0xD0, xusb2prm);

	// Enable SuperSpeed termination
	uint32_t usb3_pssen = pci_cfg_read32(virt_base, start, bus, dev, fn, 0xD8);
	pci_cfg_write32(virt_base, start, bus, dev, fn, 0xD8, usb3_pssen);

	// USB3 routing: USB3PRM -> USB3PR (older docs call it XUSB3PRM)
	uint32_t usb3prm = pci_cfg_read32(virt_base, start, bus, dev, fn, 0xDC);
	pci_cfg_write32(virt_base, start, bus, dev, fn, 0xDC, usb3prm);
}

extern "C" void kmain(void) {
	// frame buffer
	if (!LIMINE_BASE_REVISION_SUPPORTED) hcf();
	if (!fb_req.response || fb_req.response->framebuffer_count < 1) hcf();
	
	fb = fb_req.response->framebuffers[0];
	
	if (!memmap_req.response) {
		print("Error: No memmap response");
		hcf();
	}
	
	if (!hhdm_req.response) {
		print("Error: No hhdm response");
		hcf();
	}
	
	HHDM = hhdm_req.response->offset;
	// HHDM + PA = VA
	
	
	// Let's go map the memmory
	print("Setting up memory management");
//	init_va_layout();
	init_physical_allocator();
	map_region(HHDM, 0, max_hhdm_size, PRESENT | WRITABLE);
	print("Mapped the entire ram");
	
	// RSDP table (will point to the thing needed to find PCIe)
	if (!rsdp_req.response) hcf();
	uint64_t rsdp_va = (uint64_t)rsdp_req.response->address;
	
	
	volatile uint8_t *ptr = (volatile uint8_t *)rsdp_va;
	
	const char expected[8] = {'R','S','D',' ','P','T','R',' '};
	bool valid_signature = true;
	for (int i = 0; i < 8; i++) {
		if (ptr[i] != expected[i]) {
			valid_signature = false;
			break;
		}
	}
	
	if (!valid_signature) {
		print("Error: No valid signature for RSD ptr");
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
		print("Error: No valid signature for rsdt");
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
	
		if (str[0] == 'M' && str[1] == 'C' && str[2] == 'F' && str[3] == 'G') {
			ptr_mcfg = entry;
			break;
		}
	}
	
	if (ptr_mcfg == 0) {
		print("Error: ptr_mcfg == 0");
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
		
		print("Mapping ECAM");
		
		uint64_t ecam_size = (uint64_t)(end - start + 1) * 0x100000ULL; // 1MiB per bus
		uint64_t virt_base = PCI_ECAM_VA_BASE + (uint64_t)seg * PCI_ECAM_SEG_STRIDE;
		// Map MMIO as UC:
		map_ecam_region(phys_base, virt_base, ecam_size);
		print("Mapped...");
		
		for (uint8_t bus = start; bus <= end; bus++) {
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
			if (usb_found) { break; }
		}
	}
	
	if (!usb_found) {
		print("Could not find a USB controller!");
		hcf();
	}
	
	
	char str[64];
	to_hex(usb_prog_if, str);
	print("Prog_If:");
	print(str);
	
	if (usb_prog_if != 0x30) {
		print("Unsupported USB protocol.");
		hcf();
	}
	
	// Read vendor/device id to decide whether to apply Intel routing
	uint32_t vid_did = pci_cfg_read32(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x00);
	uint16_t vendor  = (uint16_t)(vid_did & 0xFFFF);
	
	// Enable Memory Space + **Bus Mastering** (needed for DMA)
	uint16_t pcmd = pci_cfg_read16(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x04);
	pcmd |= (1u<<1) | (1u<<2); // MSE | BME
	pci_cfg_write16(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x04, pcmd);
	
	// Intel quirk: route ports the BIOS allows to xHCI
	if (vendor == 0x8086) {
		print((char*)"Applying Intel xHCI routing...");
		intel_route_all_ports(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn);
	}
	
	
	// now that we have the usb device here, we need to identify the location of the bar addresses.
	// essentially a bar is a register holding the loaction that will store the space in which we will communicate with the device.
	// usb uses 1, so does nvme ssd. However, network might use more than one, vga/gpu will use several. Right now let's focus only on usb
	
	uint32_t bar0_low  = pci_cfg_read32(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x10);
	uint32_t bar1_high = pci_cfg_read32(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x14);
	
	uint64_t bar_addr = ((uint64_t)bar1_high << 32) | (bar0_low & ~0xFULL);
	
	
	
	
	uint16_t before = pci_cfg_read16(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x04);
	pci_cfg_write16(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x04, before | (1 << 1));
	uint16_t after = pci_cfg_read16(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x04);
	
	// Save old value
	pci_cfg_write32(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x10, 0xFFFFFFFF);
	uint32_t size_mask = pci_cfg_read32(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x10);
	pci_cfg_write32(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x10, bar0_low); // restore
	
	uint64_t mmio_size = ~(size_mask & ~0xF) + 1;
	
	map_mmio_region(bar_addr, USB_VA_BASE, mmio_size);
	
	print("Mapped USB");
	
	volatile uint32_t* read = (volatile uint32_t*)USB_VA_BASE;
	
	uint32_t info = read[0];
	to_hex(info, str); print(str);
	to_hex((info>>16)&0xFFFF, str); print(str);
	
	uint32_t caplen = (info & 0xFF);
	uint32_t ver = (info>>16) & 0xFFFF;
	uint32_t hcs1 = read[1] & 0xFFFFFFFF;
	uint32_t hcs2 = read[2] & 0xFFFFFFFF;
	uint32_t hcs3 = read[3] & 0xFFFFFFFF;
	uint32_t hcc1 = read[4] & 0xFFFFFFFF;
	uint32_t dboff = read[5] & 0xFFFFFFFF;
	uint32_t rtsoff = read[6] & 0xFFFFFFFF;
	uint32_t hccparams2 = read[7] & 0xFFFFFFFF;
	
	
	xhci_legacy_handoff(hcc1, USB_VA_BASE);
	
	
	print("XHCI found, version: ");
	to_str(ver, str);
	print(str);
	print("Cap len: ");
	to_str(caplen, str);
	print(str);
		
	volatile XHCIOpRegs* ops = (volatile XHCIOpRegs*)((uintptr_t)USB_VA_BASE + caplen);
	
	// Doorbell and Runtime bases per spec (32-bit DB registers!)
	volatile uint32_t* doorbell32 = (volatile uint32_t*)(USB_VA_BASE + (dboff & ~0x3));
	volatile uint8_t*  rt_base    = (volatile uint8_t*)(USB_VA_BASE + (rtsoff & ~0x1F));
	
	// Interrupter 0 regs at rt_base + 0x20
	volatile uint32_t* iman   = (volatile uint32_t*)(rt_base + 0x20 + 0x00);
	volatile uint32_t* imod   = (volatile uint32_t*)(rt_base + 0x20 + 0x04);
	volatile uint32_t* erstsz = (volatile uint32_t*)(rt_base + 0x20 + 0x08);
	volatile uint64_t* erstba = (volatile uint64_t*)(rt_base + 0x20 + 0x10);
	volatile uint64_t* erdp   = (volatile uint64_t*)(rt_base + 0x20 + 0x18);
	
	// --- Reset sequence ---
	while (!(ops->usbsts & 1)) { /* ensure halted */ }
	ops->usbcmd |= (1u << 1);     // HCRST
	while (ops->usbcmd & (1u << 1)) { /* wait reset complete */ }
	while (ops->usbsts & (1u << 11)) { /* CNR clears */ }
	
	print("Controller reset complete!");
	
	// --- Build Command Ring ---
	struct Ring cr = {};
	ring_init(&cr);
	ops->crcr = (cr.phys & ~0x3FULL) | 1;      // RCS=1
	
	// --- Build Event Ring + ERST (single segment) ---
	volatile TRB* er_virt = (volatile TRB*)alloc_table();
	uint64_t er_phys = ((uint64_t)er_virt) - HHDM;
	// Event ring entries start with Cycle=0; controller will write with CCS=1
	for (int i = 0; i < 256; i++) {
		er_virt[i].parameter = 0;
		er_virt[i].status    = 0;
		er_virt[i].control   = 0; // Cycle=0
	}
	
	volatile ERSTEntry* erst = (volatile ERSTEntry*)alloc_table();
	uint64_t erst_phys = ((uint64_t)erst) - HHDM;
	
	erst[0].ring_segment_base = er_phys;
	erst[0].ring_segment_size = 256;
	erst[0].reserved          = 0;
	
	// Program Interrupter 0 ERST/ERDP
	*erstsz = 1;
	*erstba = erst_phys;
	*erdp   = er_phys; // dequeue pointer at start
	
	// Enable interrupter (optional for polling, but correct bit)
	*iman |= (1u << 1); // IE=1
	
	// --- DCBAA & CONFIG, including Scratchpad if required ---
	uint32_t max_slots = hcs1 & 0xFF;
	
	volatile uint64_t* dcbaa_virt = (volatile uint64_t*)alloc_table(); // 4K page
	uint64_t dcbaa_phys = ((uint64_t)dcbaa_virt) - HHDM;
	for (uint32_t i = 0; i < max_slots; i++) { dcbaa_virt[i] = 0; }
	
	// Scratchpad requirement from HCSPARAMS2:
	// total_scratch = (MaxScratchpadHi << 5) | MaxScratchpadLo
//	uint32_t hcs2 = /* your code should have read this earlier */ (uint32_t)hcs2;
	uint32_t sp_lo = (hcs2 & 0x1F);             // bits 4:0
	uint32_t sp_hi = (hcs2 >> 27) & 0x1F;       // bits 31:27
	uint32_t sp_count = (sp_hi << 5) | sp_lo;
	
	if (sp_count) {
		// Allocate Scratchpad Buffer Array (list of physical addrs to pages)
		volatile uint64_t* sp_array_virt = (volatile uint64_t*)alloc_table();
		uint64_t sp_array_phys = ((uint64_t)sp_array_virt) - HHDM;
	
		for (uint32_t i = 0; i < sp_count; i++) {
			// one 4KiB scratch page each (could be larger page sizes if supported)
			volatile uint8_t* page = (volatile uint8_t*)alloc_table();
			uint64_t page_phys = ((uint64_t)page) - HHDM;
			sp_array_virt[i] = page_phys;
		}
		// DCBAA[0] must point to Scratchpad Buffer Array when present
		dcbaa_virt[0] = sp_array_phys;
	}
	
	ops->dcbaap = dcbaa_phys;
	ops->config = max_slots;
	
	// --- Run the controller ---
	ops->usbcmd |= 1u;                 // Run/Stop = 1
	while (ops->usbsts & 1u) { /* wait HCHalted==0 */ }
	
	// Re-write CRCR after run (harmless; keeps you consistent with some drivers)
	ops->crcr = (cr.phys & ~0x3FULL) | 1;
	
	print("Controller running!");
	
	// --- Port loop & Enable Slot command ---
	// --- Port loop & Enable Slot command (FIXED) ---
	uint8_t max_ports = (hcs1 >> 24) & 0xFF;
	uint8_t ccs = 1; // Consumer Cycle State for event ring
	uint8_t erdp_index = 0;
	
	// Check if Port Power Control is needed
	bool needs_ppc = (hcc1 & (1u << 3));
	
	if (needs_ppc) {
		print("We will be powering ports manually.");
	}
	
	
	print("Waiting for device connections...");
	size_t timeout = 5000000;  // Wait up to ~5 seconds
	while (timeout-- > 0) {
		TRB* evt = (TRB*)&er_virt[erdp_index];
		uint32_t ctrl = evt->control;
		uint32_t cyc = ctrl & 1u;
		
		if (cyc == ccs) {
			uint32_t type = (ctrl >> 10) & 0x3F;
			
			if (type == 34) {
				// Handle PSC event
				uint32_t port_id = (evt->parameter >> 24) & 0xFF;
				print("Port Status Change on port ");
				to_str(port_id, str); print(str);
				
				volatile uint32_t* portsc = (volatile uint32_t*)((uintptr_t)ops + 0x400 + (port_id-1) * 0x10);
				uint32_t psc = *portsc;
				
				// Check if device connected
				if (psc & 1u) {  // CCS bit
					print("Device NOW connected!");
					// Clear CSC bit
					*portsc |= (1u << 17);
					// Re-run your port enumeration logic here
				}
			} else {
				print("Event type: ");
				to_str(type, str);
				print(str);
			}
			
			// **CRITICAL: Advance ERDP after consuming ANY event**
			erdp_index = (erdp_index + 1) % 256;
			if (erdp_index == 0) ccs ^= 1;  // Toggle cycle state when wrapping
			
			uint64_t new_erdp = er_phys + (erdp_index * 16);
			*erdp = new_erdp | (1ull << 3);  // Write new ERDP with EHB bit
		}
		
		for (volatile int i = 0; i < 1000; i++);
	}
	
	print("Scanning ports...");
	
	// PORTSC helpers
	constexpr uint32_t PORTSC_CCS = 1u << 0;
	constexpr uint32_t PORTSC_PED = 1u << 1;
	constexpr uint32_t PORTSC_PR  = 1u << 4;
	constexpr uint32_t PORTSC_PLS_MASK = 0xFu << 5;   // bits 5..8
	constexpr uint32_t PORTSC_PP  = 1u << 9;
	constexpr uint32_t PORTSC_PS_MASK  = 0xFu << 10;  // bits 10..13
	constexpr uint32_t PORTSC_PIC_MASK = 0x3u << 14;  // bits 14..15
	constexpr uint32_t PORTSC_LWS = 1u << 16;
	constexpr uint32_t PORTSC_W1C = (1u<<17)|(1u<<18)|(1u<<19)|(1u<<20)|
									(1u<<21)|(1u<<22)|(1u<<23); // CSC..CEC
	
	// Clear any *set* change bits (W1C), preserving everything else:
	auto ack_port_changes = [&](volatile uint32_t* portsc) {
		uint32_t v = *portsc;
		uint32_t w = (v & ~PORTSC_W1C) | (v & PORTSC_W1C); // write 1s only where bits are set
		*portsc = w;
	};
	
	// Issue port reset *without* dropping PP/PLS/PS/PIC:
	auto port_reset = [&](volatile uint32_t* portsc) -> bool {
		uint32_t v = *portsc;
	
		// If you have PPC, make sure PP is on before anything else.
		v |= PORTSC_PP;
	
		// Do not accidentally clear W1C bits when setting PR.
		uint32_t w = (v & ~PORTSC_W1C) | PORTSC_PR;
		*portsc = w;
	
		// Wait for PR to clear (reset complete)
		int timeout = 100000;
		while ((*portsc & PORTSC_PR) && --timeout) { __asm__ __volatile__("pause"); }
		if (!timeout) return false;
	
		// After PR clears, the xHC should set PED=1 and PRC=1. Ack PRC.
		uint32_t v2 = *portsc;
		uint32_t w2 = (v2 & ~PORTSC_W1C) | (1u<<21); // PRC W1C
		*portsc = w2;
		return true;
	};
	
	
	for (uint32_t i = 0; i < max_ports; i++) {
		volatile uint32_t* portsc = (volatile uint32_t*)((uintptr_t)ops + 0x400 + i * 0x10);
		
		if (needs_ppc) {
			*portsc |= PORTSC_PP;
			spin_delay(50*1000);
		}
		
		ack_port_changes(portsc);
		
		uint32_t v = *portsc;
		if (!(v & PORTSC_CCS)) continue;
		
		if (!port_reset(portsc)) {
			print((char*)"Port reset timeout");
			continue;
		}
		
		uint32_t v_after = *portsc;
		if (!(v_after & PORTSC_CCS)) {
			print((char*)"Port not enabled after reset; PORTSC:");
			to_hex(v_after, str); print(str);
			continue;
		}
		
		ring_push_cmd(&cr, (9u<<10), 0, 0);
		doorbell32[0] = 0;
		
		timeout = 1000000;
		bool got_response = false;
		uint32_t slot_id;
		
		while (timeout-- > 0) {
			TRB* evt = (TRB*)&er_virt[erdp_index];
			uint32_t ctrl = evt->control;
			uint32_t cyc = ctrl & 1u;
			
			if (cyc != ccs) {
				// Event not ready yet
				for (volatile int i = 0; i < 10; i++);
				continue;
			}
			
			uint32_t type = (ctrl >> 10) & 0x3F;
			print("Event type:");
			to_str(type, str);
			print(str);
			
			if (type == 34) {
				print("Port Status Change Event");
				
				erdp_index = (erdp_index + 1) % 256;
				if (erdp_index == 0) ccs ^= 1;
				
				uint64_t new_erdp = er_phys + (erdp_index * 16);
				*erdp = new_erdp | (1ull << 3);
				continue;
			}else if (type == 33) {
				uint32_t code = (evt->status >> 24) & 0xFF;
				print("Completion code:");
				to_str(code, str); 
				print(str);
				
				if (code == 1) {  // Success
					slot_id = (ctrl >> 24) & 0xFF;
					print("Slot enabled! ID="); 
					to_str(slot_id, str); 
					print(str);
				} else {
					print("Enable Slot FAILED with code:");
					to_str(code, str);
					print(str);
				}
				
				// Advance ERDP
				erdp_index = (erdp_index + 1) % 256;
				if (erdp_index == 0) ccs ^= 1;
				
				uint64_t new_erdp = er_phys + (erdp_index * 16);
				*erdp = new_erdp | (1ull << 3);
				
				got_response = true;
				break;
			}else {
				print("Unexpected event type");
				
				erdp_index = (erdp_index + 1) % 256;
				if (erdp_index == 0) ccs ^= 1;
				
				uint64_t new_erdp = er_phys + (erdp_index * 16);
				*erdp = new_erdp | (1ull << 3);
			}
		}
		
		if (!got_response) {
			print("Timeout waiting for Enable Slot response");
			continue;
		}
		
		
		
		
		
		// here we have the item that seems to be ready?
		
		struct Ring ep0;
		ring_init(&ep0);
		uint64_t ep0_ring_phys = ep0.phys;
		
		
		uint32_t speed = (v >> 10) & 0xF;
		
		volatile uint64_t* input_ctx_virt = alloc_table();
		uint32_t input_ctx_phys = (uint64_t)input_ctx_virt-HHDM;
		
		
		volatile InputControlCtx *data = (volatile InputControlCtx*)input_ctx_virt;
		data->add_flags = (1 << 0) | (1 << 1);
		
		volatile SlotCtx* slot_ctx = (volatile SlotCtx*)(input_ctx_virt+0x20);
		
		slot_ctx->route_string = 0;
		slot_ctx->speed = speed; // e.g. 3 = full speed, 4 = high speed, etc.
		slot_ctx->port_num = i + 1;  // 1-based per xHCI spec
		
		volatile struct EndpointCtx *ep0_ctx = (volatile struct EndpointCtx *)((uintptr_t)input_ctx_virt + 0x40);
		
		#define EP_TYPE_CONTROL 4
		
		ep0_ctx->ep_state = 0;
		ep0_ctx->ep_flags =
			(EP_TYPE_CONTROL << 3) |      // Endpoint Type (Control)
			(64 << 16);                   // Max Packet Size (HS)
		ep0_ctx->tr_deq_ptr = ep0_ring_phys | 1; // bit 0 = DCS
		ep0_ctx->avg_trb_len = 8;
		ep0_ctx->max_esit_hi = 0;
		
		ring_push_cmd(&cr, (11u << 10) | (slot_id << 24), 
		              (uint32_t)(input_ctx_phys & 0xFFFFFFFF),
		              (uint32_t)(input_ctx_phys >> 32));
		doorbell32[0] = 0;
	}
	
	print("Port scan complete.");
	
	print("Finished.");
	hcf();
}