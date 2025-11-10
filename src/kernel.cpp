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

// USB HID keycodes to ASCII (unshifted)
static const char hid_to_ascii[256] = {
	0, 0, 0, 0, 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
	'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
	'1', '2', '3', '4', '5', '6', '7', '8', '9', '0',
	'\n', 27, '\b', '\t', ' ', '-', '=', '[', ']', '\\', 0, ';', '\'', '`', ',', '.', '/',
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // F-keys
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // more
};

// USB HID keycodes to ASCII (shifted)
static const char hid_to_ascii_shift[256] = {
	0, 0, 0, 0, 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
	'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
	'!', '@', '#', '$', '%', '^', '&', '*', '(', ')',
	'\n', 27, '\b', '\t', ' ', '_', '+', '{', '}', '|', 0, ':', '"', '~', '<', '>', '?',
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
	uint64_t reserved;
	struct MCFGEntry entries[];
} __attribute__((packed));

inline volatile uint32_t* pci_cfg_ptr32(uint64_t virt_base, uint8_t start_bus, uint8_t bus, uint8_t dev, uint8_t fn, uint16_t off) {
	if ((off & 3) || off > 0xFFC) hcf();
	uint64_t addr = virt_base
				  + ((uint64_t)(bus - start_bus) << 20)
				  + ((uint64_t)dev << 15)
				  + ((uint64_t)fn  << 12)
				  + off;
	return (volatile uint32_t*)addr;
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
int max_print_lines = 0;

void print(char* buffer) {
	// Calculate max lines based on screen height
	if (max_print_lines == 0) {
		max_print_lines = fb->height / 10;  // 10 pixels per line
	}
	
	// Wrap around to top if we've reached the bottom
	if (current_print_line >= max_print_lines) {
		current_print_line = 0;
		// Clear the screen
		fill_screen_fast(0x00000000);
	}
	
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

#define PAGE_SIZE 0x1000

static inline uint64_t read_cr3() {
	uint64_t value;
	asm volatile ("mov %%cr3, %0" : "=r"(value));
	return value;
}

static uint64_t next_free_phys_page = 0;
static uint64_t max_free_phys_page = 0;
const uint64_t GUARD = 2*1024*1024;
static uint64_t max_hhdm_size = 0;

void init_physical_allocator() {
	auto memmap = memmap_req.response;
	uint64_t best_len = 0;
	uint64_t best_base = 0;

	for (uint64_t i = 0; i < memmap->entry_count; i++) {
		auto *entry = memmap->entries[i];
		if (entry->type == LIMINE_MEMMAP_USABLE && entry->length > best_len) {
			best_len = entry->length;
			best_base = (entry->base + PAGE_SIZE - 1) & ~(PAGE_SIZE - 1);
		}
		if (entry->length + entry->base > max_hhdm_size) {
			max_hhdm_size = entry->length + entry->base;
		}
	}

	next_free_phys_page = best_base;
	max_free_phys_page = best_base+best_len;
	
	char str[64];
	to_hex(next_free_phys_page, str);
	print((char*)"Next free phys page");
	print(str);
	
	to_hex(max_free_phys_page, str);
	print((char*)"Max free phys page");
	print(str);
}

uint64_t alloc_phys_page() {
	if (next_free_phys_page >= max_free_phys_page) {
		print((char*)"OUT OF MEMORY!");
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

static inline volatile uint64_t *descend(volatile uint64_t *parent, size_t idx, int level) {
	uint64_t e = parent[idx];

	if (!(e & PRESENT)) {
		volatile uint64_t *child = alloc_table();
		uint64_t phys = (uint64_t)child - HHDM;
		parent[idx] = (phys & PTE_ADDR_MASK) | PRESENT | WRITABLE;
		return child;
	}

	if (level == 3 && (e & HUGE)) {
		uint64_t flags = e & ~(PTE_ADDR_MASK | HUGE);
		uint64_t base  = e & PDP_1G_ADDR_MASK;
		volatile uint64_t *pd = alloc_table();
		for (int i = 0; i < 512; i++)
			pd[i] = (base + ((uint64_t)i << 21)) | flags | PRESENT | HUGE;
		uint64_t phys = (uint64_t)pd - HHDM;
		parent[idx] = (phys & PTE_ADDR_MASK) | (flags & ~HUGE) | PRESENT | WRITABLE;
		return pd;
	}

	if (level == 2 && (e & HUGE)) {
		uint64_t flags = e & ~(PTE_ADDR_MASK | HUGE);
		uint64_t base  = e & PD_2M_ADDR_MASK;
		volatile uint64_t *pt = alloc_table();
		for (int i = 0; i < 512; i++)
			pt[i] = (base + ((uint64_t)i << 12)) | flags | PRESENT;
		uint64_t phys = (uint64_t)pt - HHDM;
		parent[idx] = (phys & PTE_ADDR_MASK) | (flags & ~HUGE) | PRESENT | WRITABLE;
		return pt;
	}

	return (volatile uint64_t *)(HHDM + (e & PTE_ADDR_MASK));
}

void map_page(uint64_t va, uint64_t pa, uint64_t flags) {
	volatile uint64_t *pml4 = (volatile uint64_t *)(HHDM + (read_cr3() & PTE_ADDR_MASK));
	volatile uint64_t *pdpt = descend(pml4, PML4_INDEX(va), 4);
	volatile uint64_t *pd   = descend(pdpt, PDPT_INDEX(va), 3);
	volatile uint64_t *pt   = descend(pd,   PD_INDEX(va),   2);
	
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
		map_page(virt_base + offset, phys_base + offset, WRITABLE | CACHE_DISABLE | WRITE_THROUGH | NX);
	}
}

void map_region(uint64_t virt_start, uint64_t phys_start, size_t length, uint64_t flags) {
	size_t pages = (length + 0xFFF) / 0x1000;
	
	for (size_t i = 0; i < pages; i++) {
		map_page(virt_start + i * 0x1000, phys_start + i * 0x1000, flags);
	}
}

uint64_t PCI_ECAM_VA_BASE = 0xFFFF'8000'0000'0000ULL;
uint64_t PCI_ECAM_SEG_STRIDE = 0x10000000ULL;

uint64_t PCI_MMIO_VA_BASE = 0xFFFF'9000'0000'0000ULL;
uint64_t USB_VA_BASE = (PCI_MMIO_VA_BASE + 0x0010'0000ULL);

struct XHCIOpRegs {
	volatile uint32_t usbcmd;
	volatile uint32_t usbsts;
	volatile uint32_t pagesize;
	volatile uint8_t  reserved1[8];
	volatile uint32_t dnctrl;
	volatile uint64_t crcr;
	volatile uint8_t  reserved2[16];
	volatile uint64_t dcbaap;
	volatile uint32_t config;
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
	volatile TRB *trb;
	uint64_t phys;
	uint32_t enq;
	uint8_t  pcs;
};

struct InputControlCtx {
	uint32_t drop_flags;
	uint32_t add_flags;
	uint32_t rsvd[6];
};

struct SlotCtx {
	uint32_t route_string;
	uint32_t speed;
	uint32_t rsvd2;
	uint32_t rsvd3;
	uint32_t tt_hub_slotid;
	uint32_t tt_portnum;
	uint32_t port_num;
	uint32_t rsvd4;
};

struct EndpointCtx {
	uint32_t ep_state;
	uint32_t ep_flags;
	uint64_t tr_deq_ptr;
	uint32_t avg_trb_len;
	uint32_t max_esit_hi;
	uint32_t reserved[2];
} __attribute__((packed));

void ring_init(struct Ring *r) {
	r->trb = (volatile TRB*)alloc_table();
	r->phys = (uint64_t)r->trb - HHDM;
	r->enq = 0;
	r->pcs = 1;

	for (int i = 0; i < 256; i++) r->trb[i].control = 0;

	r->trb[255].parameter = r->phys;
	r->trb[255].status = 0;
	r->trb[255].control = (6u<<10) | (1u<<1);
}

void ring_push_cmd(struct Ring *r, uint32_t ctrl, uint64_t param, uint32_t status) {
	volatile TRB *t = &r->trb[r->enq];
	t->parameter = param;
	t->status    = status;
	t->control   = (ctrl & ~1u) | r->pcs;

	if (++r->enq == 255) {
		r->enq = 0;
		r->pcs ^= 1;
	}
}

static inline void spin_delay(volatile uint64_t iters) {
	for (volatile uint64_t i=0; i<iters; ++i) { __asm__ __volatile__("pause"); }
}

static void xhci_legacy_handoff(uint32_t hcc1, uint64_t mmio_base) {
	uint32_t xecp_dw = (hcc1 >> 16) & 0xFFFF;
	while (xecp_dw) {
		volatile uint32_t *ec = (volatile uint32_t*)(mmio_base + (uint64_t)xecp_dw*4);
		uint32_t hdr   = ec[0];
		uint32_t capid =  hdr        & 0xFF;
		uint32_t next  = (hdr >> 8)  & 0xFF;

		if (capid == 1) {
			volatile uint32_t *usblegsup    = ec + 0;
			volatile uint32_t *usblegctlsts = ec + 1;

			*usblegsup |= (1u << 24);
			for (int t=0; t<1000000 && (*usblegsup & (1u<<16)); ++t) { __asm__ __volatile__("pause"); }

			*usblegctlsts = 0;
			*usblegctlsts = 0xFFFFFFFF;
			break;
		}
		xecp_dw = next;
	}
}

static void intel_route_all_ports(uint64_t virt_base,
								  uint8_t start, uint8_t bus, uint8_t dev, uint8_t fn)
{
	uint32_t xusb2prm = pci_cfg_read32(virt_base, start, bus, dev, fn, 0xD4);
	pci_cfg_write32(virt_base, start, bus, dev, fn, 0xD0, xusb2prm);

	uint32_t usb3_pssen = pci_cfg_read32(virt_base, start, bus, dev, fn, 0xD8);
	pci_cfg_write32(virt_base, start, bus, dev, fn, 0xD8, usb3_pssen);

	uint32_t usb3prm = pci_cfg_read32(virt_base, start, bus, dev, fn, 0xDC);
	pci_cfg_write32(virt_base, start, bus, dev, fn, 0xDC, usb3prm);
}

static inline uint64_t align_down(uint64_t x, uint64_t a){ return x & ~(a-1); }
static inline uint64_t align_up  (uint64_t x, uint64_t a){ return (x + a - 1) & ~(a-1); }

static void map_2m(uint64_t va, uint64_t pa, uint64_t flags) {
	volatile uint64_t *pml4 = (volatile uint64_t *)(HHDM + (read_cr3() & PTE_ADDR_MASK));
	volatile uint64_t *pdpt = descend(pml4, PML4_INDEX(va), 4);
	volatile uint64_t *pd   = descend(pdpt, PDPT_INDEX(va), 3);
	pd[PD_INDEX(va)] = (pa & PD_2M_ADDR_MASK) | (flags | PRESENT | WRITABLE | HUGE);
	asm volatile("invlpg (%0)" :: "r"(va) : "memory");
}

static void map_range_huge(uint64_t va, uint64_t pa, uint64_t len, uint64_t flags) {
	while (len >= (1ull<<21) &&
		   ((va | pa) & ((1ull<<21)-1)) == 0) {
		map_2m(va, pa, flags);
		va += (1ull<<21); pa += (1ull<<21); len -= (1ull<<21);
	}
	while (len) {
		map_page(va, pa, flags);
		va += 0x1000; pa += 0x1000; len -= 0x1000;
	}
}

static void map_hhdm_usable(uint64_t flags) {
	map_range_huge(HHDM, 0, max_hhdm_size, flags);
}

// USB Setup packet
struct USBSetupPacket {
	uint8_t bmRequestType;
	uint8_t bRequest;
	uint16_t wValue;
	uint16_t wIndex;
	uint16_t wLength;
} __attribute__((packed));

// Device state
struct USBDevice {
	uint8_t slot_id;
	uint8_t port_num;
	uint32_t speed;
	struct Ring ep0_ring;
	struct Ring kbd_ring;
	volatile uint64_t* device_ctx;
};

extern "C" void kmain(void) {
	if (!LIMINE_BASE_REVISION_SUPPORTED) hcf();
	if (!fb_req.response || fb_req.response->framebuffer_count < 1) hcf();
	
	fb = fb_req.response->framebuffers[0];
	
	if (!memmap_req.response) {
		print((char*)"Error: No memmap response");
		hcf();
	}
	
	if (!hhdm_req.response) {
		print((char*)"Error: No hhdm response");
		hcf();
	}
	
	HHDM = hhdm_req.response->offset;
	
	print((char*)"Setting up memory management");
	init_physical_allocator();
	map_hhdm_usable(PRESENT | WRITABLE);
	
	print((char*)"Mapped the entire ram");
	
	if (!rsdp_req.response) hcf();
	uint64_t rsdp_va = (uint64_t)rsdp_req.response->address;
	
	volatile uint8_t *ptr = (volatile uint8_t*)rsdp_va;
	
	const char expected[8] = {'R','S','D',' ','P','T','R',' '};
	bool valid_signature = true;
	for (int i = 0; i < 8; i++) {
		if (ptr[i] != expected[i]) {
			valid_signature = false;
			break;
		}
	}
	
	if (!valid_signature) {
		print((char*)"Error: No valid signature for RSD ptr");
		hcf();
	}
	
	uint32_t rsdt_pa_32 =
		(uint32_t)ptr[0x10] |
		((uint32_t)ptr[0x11] << 8) |
		((uint32_t)ptr[0x12] << 16) |
		((uint32_t)ptr[0x13] << 24);
	
	uint64_t rsdt_va = HHDM+(uint64_t)rsdt_pa_32;
	volatile uint8_t *ptr_rsdt = (volatile uint8_t *)rsdt_va;
	
	const char expected_rsdt[4] = {'R','S','D','T'};
	valid_signature = true;
	for (int i = 0; i < 4; i++) {
		if (ptr_rsdt[i] != expected_rsdt[i]) {
			valid_signature = false;
			break;
		}
	}
	if (!valid_signature) {
		print((char*)"Error: No valid signature for rsdt");
		hcf();
	}
	
	uint32_t rsdt_length =
		(uint32_t)ptr_rsdt[4] |
		((uint32_t)ptr_rsdt[5] << 8) |
		((uint32_t)ptr_rsdt[6] << 16) |
		((uint32_t)ptr_rsdt[7] << 24);
	uint32_t entry_count = (rsdt_length - 36) / 4;
	
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
		print((char*)"Error: ptr_mcfg == 0");
		hcf();
	}
	
	volatile struct MCFGHeader *mcfg = (volatile struct MCFGHeader *)ptr_mcfg;
	
	uint32_t length = mcfg->header.length;
	uint32_t entries = (length - sizeof(struct ACPISDTHeader) - 8) / 16;
	
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
		
		print((char*)"Mapping ECAM");
		
		uint64_t ecam_size = (uint64_t)(end - start + 1) * 0x100000ULL;
		uint64_t virt_base = PCI_ECAM_VA_BASE + (uint64_t)seg * PCI_ECAM_SEG_STRIDE;
		map_ecam_region(phys_base, virt_base, ecam_size);
		print((char*)"Mapped...");
		
		for (uint8_t bus = start; bus <= end; bus++) {
			for (uint8_t dev = 0; dev < 32; dev++) {
				for (uint8_t fn = 0; fn < 8; fn++) {
					uint32_t id = pci_cfg_read32(virt_base, start, bus, dev, fn, 0x00);
					if (id == 0xFFFFFFFF) continue;
					
					uint32_t class_reg = pci_cfg_read32(virt_base, start, bus, dev, fn, 0x08);
					
					uint8_t baseclass = (class_reg >> 24) & 0xFF;
					uint8_t subclass  = (class_reg >> 16) & 0xFF;
					uint8_t prog_if   = (class_reg >> 8) & 0xFF;
					
					if (baseclass == 0x0C && subclass == 0x03) {
						usb_virt_base = virt_base;
						usb_start = start;
						usb_bus = bus;
						usb_dev = dev;
						usb_fn = fn;
						usb_prog_if = prog_if;
						usb_found = true;
						print((char*)"Found a USB controller!");
						break;
					}
				}
				if (usb_found) { break; }
			}
			if (usb_found) { break; }
		}
		if (usb_found) { break; }
	}
	
	print((char*)"Got out of the stupid loop.");
	
	if (!usb_found) {
		print((char*)"Could not find a USB controller!");
		hcf();
	}
	
	print((char*)"allocing str");
	char* str = (char*)alloc_table();
	print((char*)"convert to hex");
	to_str(usb_prog_if, str);
	print((char*)"Prog_If:");
	print(str);
	
	if (usb_prog_if != 0x30) {
		print((char*)"Unsupported USB protocol.");
		hcf();
	}
	
	uint32_t vid_did = pci_cfg_read32(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x00);
	uint16_t vendor  = (uint16_t)(vid_did & 0xFFFF);
	
	uint16_t pcmd = pci_cfg_read16(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x04);
	pcmd |= (1u<<1) | (1u<<2);
	pci_cfg_write16(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x04, pcmd);
	
	if (vendor == 0x8086) {
		print((char*)"Applying Intel xHCI routing...");
		intel_route_all_ports(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn);
	}
	
	uint32_t bar0 = pci_cfg_read32(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x10);

	if ((bar0 & 0x1) == 1) {
		print((char*)"IO space bar err");
		hcf();
	}
	
	uint32_t bar_type = (bar0 >> 1) & 0x3;
	uint64_t bar_addr;
	
	if (bar_type == 0x2) {
		uint32_t bar1_high = pci_cfg_read32(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x14);
		bar_addr = ((uint64_t)bar1_high << 32) | (bar0 & ~0xFULL);
	} else {
		bar_addr = bar0 & ~0xFULL;
	}
	
	uint16_t before = pci_cfg_read16(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x04);
	pci_cfg_write16(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x04, before | (1 << 1));
	uint16_t after = pci_cfg_read16(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x04);
	
	pci_cfg_write32(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x10, 0xFFFFFFFF);
	uint32_t size_mask = pci_cfg_read32(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x10);
	pci_cfg_write32(usb_virt_base, usb_start, usb_bus, usb_dev, usb_fn, 0x10, bar0);
	
	uint64_t mmio_size = ~(size_mask & ~0xF) + 1;
	
	to_str(mmio_size, str);
	print(str);
	to_hex(bar_addr, str);
	print(str);
	
	map_mmio_region(bar_addr, USB_VA_BASE, mmio_size);
	
	print((char*)"Mapped USB");
	
	volatile uint32_t* read = (volatile uint32_t*)USB_VA_BASE;
	
	print((char*)"TEST _COMMENT");
	
	uint32_t info = read[0];
	
	print((char*)"Test_3");
	
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
	
	print((char*)"XHCI found, version: ");
	to_str(ver, str);
	print(str);
	print((char*)"Cap len: ");
	to_str(caplen, str);
	print(str);
	
	volatile XHCIOpRegs* ops = (volatile XHCIOpRegs*)((uintptr_t)USB_VA_BASE + caplen);
	
	volatile uint32_t* doorbell32 = (volatile uint32_t*)(USB_VA_BASE + (dboff & ~0x3));
	volatile uint8_t*  rt_base    = (volatile uint8_t*)(USB_VA_BASE + (rtsoff & ~0x1F));
	
	volatile uint32_t* iman   = (volatile uint32_t*)(rt_base + 0x20 + 0x00);
	volatile uint32_t* imod   = (volatile uint32_t*)(rt_base + 0x20 + 0x04);
	volatile uint32_t* erstsz = (volatile uint32_t*)(rt_base + 0x20 + 0x08);
	volatile uint64_t* erstba = (volatile uint64_t*)(rt_base + 0x20 + 0x10);
	volatile uint64_t* erdp   = (volatile uint64_t*)(rt_base + 0x20 + 0x18);
	
	while (!(ops->usbsts & 1)) { }
	ops->usbcmd |= (1u << 1);
	while (ops->usbcmd & (1u << 1)) { }
	while (ops->usbsts & (1u << 11)) { }
	
	struct Ring cr = {};
	ring_init(&cr);
	ops->crcr = (cr.phys & ~0x3FULL) | 1;
	
	volatile TRB* er_virt = (volatile TRB*)alloc_table();
	uint64_t er_phys = ((uint64_t)er_virt) - HHDM;
	for (int i = 0; i < 256; i++) {
		er_virt[i].parameter = 0;
		er_virt[i].status    = 0;
		er_virt[i].control   = 0;
	}
	
	volatile ERSTEntry* erst = (volatile ERSTEntry*)alloc_table();
	uint64_t erst_phys = ((uint64_t)erst) - HHDM;
	
	erst[0].ring_segment_base = er_phys;
	erst[0].ring_segment_size = 256;
	erst[0].reserved          = 0;
	
	*erstsz = 1;
	*erstba = erst_phys;
	*erdp   = er_phys;
	
	*iman |= (1u << 1) | (1u << 0);  // Enable interrupts: IE bit and IP bit
	
	print((char*)"Event ring configured, IMAN:");
	to_hex(*iman, str);
	print(str);
	
	uint32_t max_slots = hcs1 & 0xFF;
	
	volatile uint64_t* dcbaa_virt = (volatile uint64_t*)alloc_table();
	uint64_t dcbaa_phys = ((uint64_t)dcbaa_virt) - HHDM;
	for (uint32_t i = 0; i < max_slots; i++) { dcbaa_virt[i] = 0; }
	
	uint32_t sp_lo = (hcs2 & 0x1F);
	uint32_t sp_hi = (hcs2 >> 27) & 0x1F;
	uint32_t sp_count = (sp_hi << 5) | sp_lo;
	
	if (sp_count) {
		volatile uint64_t* sp_array_virt = (volatile uint64_t*)alloc_table();
		uint64_t sp_array_phys = ((uint64_t)sp_array_virt) - HHDM;
	
		for (uint32_t i = 0; i < sp_count; i++) {
			volatile uint8_t* page = (volatile uint8_t*)alloc_table();
			uint64_t page_phys = ((uint64_t)page) - HHDM;
			sp_array_virt[i] = page_phys;
		}
		dcbaa_virt[0] = sp_array_phys;
	}
	
	ops->dcbaap = dcbaa_phys;
	ops->config = max_slots;
	
	ops->usbcmd |= 1u;
	while (ops->usbsts & 1u) { }
	
	ops->crcr = (cr.phys & ~0x3FULL) | 1;
	
	print((char*)"Controller running!");
	
	uint8_t max_ports = (hcs1 >> 24) & 0xFF;
	uint8_t ccs = 1;
	uint8_t erdp_index = 0;
	
	bool needs_ppc = (hcc1 & (1u << 3));
	
	if (needs_ppc) {
		print((char*)"We will be powering ports manually.");
	}
	
	print((char*)"Scanning ports...");
	
	constexpr uint32_t PORTSC_CCS = 1u << 0;
	constexpr uint32_t PORTSC_PED = 1u << 1;
	constexpr uint32_t PORTSC_PR  = 1u << 4;
	constexpr uint32_t PORTSC_PLS_MASK = 0xFu << 5;
	constexpr uint32_t PORTSC_PP  = 1u << 9;
	constexpr uint32_t PORTSC_PS_MASK  = 0xFu << 10;
	constexpr uint32_t PORTSC_PIC_MASK = 0x3u << 14;
	constexpr uint32_t PORTSC_LWS = 1u << 16;
	constexpr uint32_t PORTSC_W1C = (1u<<17)|(1u<<18)|(1u<<19)|(1u<<20)|
									(1u<<21)|(1u<<22)|(1u<<23);
	
	auto ack_port_changes = [&](volatile uint32_t* portsc) {
		uint32_t v = *portsc;
		uint32_t w = (v & ~PORTSC_W1C) | (v & PORTSC_W1C);
		*portsc = w;
	};
	
	auto port_reset = [&](volatile uint32_t* portsc) -> bool {
		uint32_t v = *portsc;
		v |= PORTSC_PP;
		uint32_t w = (v & ~PORTSC_W1C) | PORTSC_PR;
		*portsc = w;
		int timeout = 100000;
		while ((*portsc & PORTSC_PR) && --timeout) { __asm__ __volatile__("pause"); }
		if (!timeout) return false;
		uint32_t v2 = *portsc;
		uint32_t w2 = (v2 & ~PORTSC_W1C) | (1u<<21);
		*portsc = w2;
		return true;
	};
	
	USBDevice kbd_device = {};
	bool device_found = false;
	
	for (uint32_t i = 0; i < max_ports; i++) {
		volatile uint32_t* portsc = (volatile uint32_t*)((uintptr_t)ops + 0x400 + i * 0x10);
		
		if (needs_ppc) {
			*portsc |= PORTSC_PP;
			spin_delay(50*1000);
		}
		
		ack_port_changes(portsc);
		
		uint32_t v = *portsc;
		if (!(v & PORTSC_CCS)) continue;
		
		print((char*)"Device on port ");
		to_str(i+1, str);
		print(str);
		
		if (!port_reset(portsc)) {
			print((char*)"Port reset timeout");
			continue;
		}
		
		uint32_t v_after = *portsc;
		if (!(v_after & PORTSC_PED)) {
			print((char*)"Port not enabled after reset");
			continue;
		}
		
		// Enable Slot
		ring_push_cmd(&cr, (9u<<10), 0, 0);
		doorbell32[0] = 0;
		
		size_t timeout = 1000000;
		bool got_response = false;
		uint32_t slot_id = 0;
		
		while (timeout-- > 0) {
			TRB* evt = (TRB*)&er_virt[erdp_index];
			uint32_t ctrl = evt->control;
			uint32_t cyc = ctrl & 1u;
			
			if (cyc != ccs) {
				for (volatile int j = 0; j < 10; j++);
				continue;
			}
			
			uint32_t type = (ctrl >> 10) & 0x3F;
			
			if (type == 34) {
				erdp_index = (erdp_index + 1) % 256;
				if (erdp_index == 0) ccs ^= 1;
				uint64_t new_erdp = er_phys + (erdp_index * 16);
				*erdp = new_erdp | (1ull << 3);
				continue;
			} else if (type == 33) {
				uint32_t code = (evt->status >> 24) & 0xFF;
				
				if (code == 1) {
					slot_id = (ctrl >> 24) & 0xFF;
					print((char*)"Slot enabled! ID="); 
					to_str(slot_id, str); 
					print(str);
				} else {
					print((char*)"Enable Slot FAILED");
				}
				
				erdp_index = (erdp_index + 1) % 256;
				if (erdp_index == 0) ccs ^= 1;
				uint64_t new_erdp = er_phys + (erdp_index * 16);
				*erdp = new_erdp | (1ull << 3);
				got_response = true;
				break;
			} else {
				erdp_index = (erdp_index + 1) % 256;
				if (erdp_index == 0) ccs ^= 1;
				uint64_t new_erdp = er_phys + (erdp_index * 16);
				*erdp = new_erdp | (1ull << 3);
			}
		}
		
		if (!got_response || slot_id == 0) {
			print((char*)"Timeout waiting for Enable Slot");
			continue;
		}
		
		// Setup device context
		struct Ring ep0;
		ring_init(&ep0);
		uint64_t ep0_ring_phys = ep0.phys;
		
		uint32_t speed = (v_after >> 10) & 0xF;
		
		// Allocate device output context and add to DCBAA FIRST
		volatile uint64_t* dev_ctx = alloc_table();
		uint64_t dev_ctx_phys = (uint64_t)dev_ctx - HHDM;
		dcbaa_virt[slot_id] = dev_ctx_phys;
		
		volatile uint64_t* input_ctx_virt = alloc_table();
		uint64_t input_ctx_phys = (uint64_t)input_ctx_virt - HHDM;
		
		// Clear entire input context
		for (int j = 0; j < 512; j++) {
			input_ctx_virt[j] = 0;
		}
		
		volatile InputControlCtx *icc = (volatile InputControlCtx*)input_ctx_virt;
		icc->add_flags = (1 << 0) | (1 << 1);  // Add slot + EP0
		icc->drop_flags = 0;
		
		// Slot context is at offset 0x20 (32 bytes = 4 u64s)
		volatile uint32_t* slot_ctx_dw = (volatile uint32_t*)(input_ctx_virt + 4);
		
		// DW0: route string + speed + context entries
		slot_ctx_dw[0] = 0 | ((speed & 0xF) << 20) | ((1 & 0x1F) << 27);  // 1 context entry (EP0)
		// DW1: max exit latency + root hub port + num ports
		slot_ctx_dw[1] = 0 | ((i + 1) << 16);  // Root hub port number
		// DW2-7: zeros for now
		
		// EP0 context at offset 0x40 (64 bytes = 8 u64s)
		volatile uint32_t* ep0_ctx_dw = (volatile uint32_t*)(input_ctx_virt + 8);
		
		#define EP_TYPE_CONTROL 4
		
		uint16_t max_packet = (speed == 4) ? 64 : 8;  // High speed = 64, else 8
		
		// DW0: EP state
		ep0_ctx_dw[0] = 0;
		// DW1: EP type + max packet size + max burst + error count
		ep0_ctx_dw[1] = (EP_TYPE_CONTROL << 3) | (max_packet << 16) | (3 << 1);  // CErr = 3
		// DW2-3: TR Dequeue Pointer with DCS=1
		ep0_ctx_dw[2] = (uint32_t)(ep0_ring_phys | 1);
		ep0_ctx_dw[3] = (uint32_t)(ep0_ring_phys >> 32);
		// DW4: Average TRB Length
		ep0_ctx_dw[4] = 8;
		
		// Address Device
		ring_push_cmd(&cr, (11u << 10) | (slot_id << 24), 
		              (uint32_t)(input_ctx_phys & 0xFFFFFFFF),
		              (uint32_t)(input_ctx_phys >> 32));
		doorbell32[0] = 0;
		
		timeout = 1000000;
		got_response = false;
		
		while (timeout-- > 0) {
			TRB* evt = (TRB*)&er_virt[erdp_index];
			uint32_t ctrl = evt->control;
			uint32_t cyc = ctrl & 1u;
			
			if (cyc != ccs) {
				for (volatile int j = 0; j < 10; j++);
				continue;
			}
			
			uint32_t type = (ctrl >> 10) & 0x3F;
			
			if (type == 33) {
				uint32_t code = (evt->status >> 24) & 0xFF;
				
				print((char*)"Address completion code: ");
				to_str(code, str);
				print(str);
				
				if (code == 1) {
					print((char*)"Device addressed!");
				} else {
					print((char*)"Address Device FAILED");
					to_hex(evt->parameter, str);
					print(str);
				}
				
				erdp_index = (erdp_index + 1) % 256;
				if (erdp_index == 0) ccs ^= 1;
				uint64_t new_erdp = er_phys + (erdp_index * 16);
				*erdp = new_erdp | (1ull << 3);
				got_response = true;
				break;
			} else {
				erdp_index = (erdp_index + 1) % 256;
				if (erdp_index == 0) ccs ^= 1;
				uint64_t new_erdp = er_phys + (erdp_index * 16);
				*erdp = new_erdp | (1ull << 3);
			}
		}
		
		if (!got_response) {
			print((char*)"Timeout waiting for Address Device");
			continue;
		}
		
		spin_delay(10000);  // Give device time to settle
		
		// Set Boot Protocol (HID-specific)
		volatile uint8_t* setup_buf = (volatile uint8_t*)alloc_table();
		uint64_t setup_phys = (uint64_t)setup_buf - HHDM;
		
		USBSetupPacket* setup = (USBSetupPacket*)setup_buf;
		setup->bmRequestType = 0x21;  // Host to Device, Class, Interface
		setup->bRequest = 0x0B;       // SET_PROTOCOL
		setup->wValue = 0;            // Boot protocol
		setup->wIndex = 0;            // Interface 0
		setup->wLength = 0;
		
		// Build control transfer on EP0
		// Setup Stage TRB: TRT=0 (no data), IDT=1 (immediate data in parameter)
		uint64_t setup_dw0 = ((uint64_t)setup->bmRequestType) | 
		                     ((uint64_t)setup->bRequest << 8) |
		                     ((uint64_t)setup->wValue << 16) |
		                     ((uint64_t)setup->wIndex << 32) |
		                     ((uint64_t)setup->wLength << 48);
		
		ring_push_cmd(&ep0, (2u << 10) | (8 << 16) | (1 << 6), setup_dw0, 0);  // Setup Stage, IDT=1
		
		// Status Stage TRB (IN direction, IOC=1)
		ring_push_cmd(&ep0, (4u << 10) | (1 << 16) | (1 << 5), 0, 0);  // Status IN, IOC
		
		doorbell32[slot_id] = 1;  // Ring EP0
		
		timeout = 1000000;
		got_response = false;
		
		while (timeout-- > 0) {
			TRB* evt = (TRB*)&er_virt[erdp_index];
			uint32_t ctrl = evt->control;
			uint32_t cyc = ctrl & 1u;
			
			if (cyc != ccs) {
				for (volatile int j = 0; j < 10; j++);
				continue;
			}
			
			uint32_t type = (ctrl >> 10) & 0x3F;
			
			if (type == 32 || type == 33) {  // Transfer or Command completion
				erdp_index = (erdp_index + 1) % 256;
				if (erdp_index == 0) ccs ^= 1;
				uint64_t new_erdp = er_phys + (erdp_index * 16);
				*erdp = new_erdp | (1ull << 3);
				got_response = true;
				break;
			} else {
				erdp_index = (erdp_index + 1) % 256;
				if (erdp_index == 0) ccs ^= 1;
				uint64_t new_erdp = er_phys + (erdp_index * 16);
				*erdp = new_erdp | (1ull << 3);
			}
		}
		
		print((char*)"Boot protocol set");
		
		spin_delay(10000);
		
		// Configure Endpoint for keyboard interrupt IN
		// EP1 IN = DCI 3 (formula: DCI = EP_NUM * 2 + direction, where direction is 1 for IN)
		
		print((char*)"Initializing kbd_ring...");
		ring_init(&kbd_device.kbd_ring);
		print((char*)"kbd_ring initialized");
		to_hex(kbd_device.kbd_ring.phys, str);
		print(str);
		
		volatile uint64_t* input_ctx2 = alloc_table();
		uint64_t input_ctx2_phys = (uint64_t)input_ctx2 - HHDM;
		
		// Clear entire context
		for (int j = 0; j < 512; j++) {
			input_ctx2[j] = 0;
		}
		
		volatile InputControlCtx *icc2 = (volatile InputControlCtx*)input_ctx2;
		icc2->add_flags = (1 << 0) | (1 << 3);  // Add slot context (bit 0) + EP1 IN (DCI=3, bit 3)
		icc2->drop_flags = 0;
		
		// Copy slot context from device output context
		volatile uint32_t* dev_slot_ctx = (volatile uint32_t*)dev_ctx;
		volatile uint32_t* slot_ctx2_dw = (volatile uint32_t*)(input_ctx2 + 4);
		
		// Copy existing slot context and update context entries to 3
		for (int j = 0; j < 8; j++) {
			slot_ctx2_dw[j] = dev_slot_ctx[j];
		}
		// Update context entries field to 3 (slot + EP0 + EP1)
		slot_ctx2_dw[0] = (slot_ctx2_dw[0] & ~(0x1F << 27)) | ((3 & 0x1F) << 27);
		
		// Copy EP0 context from device output context
		volatile uint32_t* dev_ep0_ctx = (volatile uint32_t*)(dev_ctx + 4);
		volatile uint32_t* ep0_ctx2_dw = (volatile uint32_t*)(input_ctx2 + 8);
		for (int j = 0; j < 8; j++) {
			ep0_ctx2_dw[j] = dev_ep0_ctx[j];
		}
		
		// Skip EP1 OUT (would be at input_ctx2 + 12)
		
		// EP1 IN context at DCI=3, which is context index 3
		// Offset: 0x20 (input control) + 3 * 0x20 = 0x80 bytes = 16 u64s
		volatile uint32_t* ep1_ctx_dw = (volatile uint32_t*)(input_ctx2 + 16);
		
		// Calculate interval for full/low speed
		// For keyboards, typically bInterval=10ms
		// Interval encoding: for Full/Low speed, it's the exponent (2^n frames)
		uint8_t interval;
		if (speed == 3 || speed == 2) {  // Full speed or Low speed
			interval = 3;  // 2^3 = 8 frames ≈ 8ms
		} else {
			interval = 4;  // For high speed, use microframes
		}
		
		// Calculate MaxPStreams (should be 0 for interrupt endpoints)
		uint32_t max_pstreams = 0;
		
		// DW0: EP state + interval + max primary streams
		ep1_ctx_dw[0] = (interval << 16) | (max_pstreams << 10);
		
		// DW1: EP type=7 (Interrupt IN) + CErr=3 + MaxPacketSize=8 + MaxBurstSize=0
		// CErr at bits 1-2, EP Type at bits 3-5, MaxBurstSize at bits 8-15, MaxPacketSize at bits 16-31
		ep1_ctx_dw[1] = (3 << 1) | (7 << 3) | (0 << 8) | (8 << 16);
		
		// DW2-3: TR Dequeue Pointer with DCS=1
		// Must be 16-byte aligned
		ep1_ctx_dw[2] = (uint32_t)(kbd_device.kbd_ring.phys | 1);
		ep1_ctx_dw[3] = (uint32_t)(kbd_device.kbd_ring.phys >> 32);
		
		// DW4: Average TRB Length
		ep1_ctx_dw[4] = 8;
		
		// DW5-7: Reserved
		ep1_ctx_dw[5] = 0;
		ep1_ctx_dw[6] = 0;
		ep1_ctx_dw[7] = 0;
		
		// Configure Endpoint command
		ring_push_cmd(&cr, (12u << 10) | (slot_id << 24), 
		              (uint32_t)(input_ctx2_phys & 0xFFFFFFFF),
		              (uint32_t)(input_ctx2_phys >> 32));
		doorbell32[0] = 0;
		
		timeout = 1000000;
		got_response = false;
		
		while (timeout-- > 0) {
			TRB* evt = (TRB*)&er_virt[erdp_index];
			uint32_t ctrl = evt->control;
			uint32_t cyc = ctrl & 1u;
			
			if (cyc != ccs) {
				for (volatile int j = 0; j < 10; j++);
				continue;
			}
			
			uint32_t type = (ctrl >> 10) & 0x3F;
			
			if (type == 33) {
				uint32_t code = (evt->status >> 24) & 0xFF;
				
				print((char*)"Configure EP code: ");
				to_str(code, str);
				print(str);
				
				if (code == 1) {
					print((char*)"Endpoint configured!");
				} else {
					print((char*)"Configure Endpoint FAILED");
					print((char*)"TRB pointer:");
					to_hex(evt->parameter, str);
					print(str);
					print((char*)"Slot ID:");
					to_str(slot_id, str);
					print(str);
					print((char*)"Input ctx phys:");
					to_hex(input_ctx2_phys, str);
					print(str);
					print((char*)"Add flags:");
					to_hex(icc2->add_flags, str);
					print(str);
					print((char*)"Drop flags:");
					to_hex(icc2->drop_flags, str);
					print(str);
					print((char*)"EP1 DW0:");
					to_hex(ep1_ctx_dw[0], str);
					print(str);
					print((char*)"EP1 DW1:");
					to_hex(ep1_ctx_dw[1], str);
					print(str);
					print((char*)"Ring phys:");
					to_hex(kbd_device.kbd_ring.phys, str);
					print(str);
				}
				
				erdp_index = (erdp_index + 1) % 256;
				if (erdp_index == 0) ccs ^= 1;
				uint64_t new_erdp = er_phys + (erdp_index * 16);
				*erdp = new_erdp | (1ull << 3);
				got_response = true;
				break;
			} else {
				erdp_index = (erdp_index + 1) % 256;
				if (erdp_index == 0) ccs ^= 1;
				uint64_t new_erdp = er_phys + (erdp_index * 16);
				*erdp = new_erdp | (1ull << 3);
			}
		}
		
		if (!got_response) {
			print((char*)"Timeout configuring endpoint");
			continue;
		}
		
		print((char*)"Endpoint configured successfully!");
		
		// Queue initial transfer requests for keyboard
		volatile uint8_t* kbd_buffer1 = (volatile uint8_t*)alloc_table();
		volatile uint8_t* kbd_buffer2 = (volatile uint8_t*)alloc_table();
		uint64_t kbd_buffer1_phys = (uint64_t)kbd_buffer1 - HHDM;
		uint64_t kbd_buffer2_phys = (uint64_t)kbd_buffer2 - HHDM;
		
		// Clear buffers
		for (int j = 0; j < 4096; j++) {
			kbd_buffer1[j] = 0;
			kbd_buffer2[j] = 0;
		}
		
		print((char*)"Queueing initial transfers...");
		to_hex(kbd_buffer1_phys, str);
		print(str);
		
		print((char*)"kbd_ring enq:");
		to_str(kbd_device.kbd_ring.enq, str);
		print(str);
		
		print((char*)"kbd_ring pcs:");
		to_str(kbd_device.kbd_ring.pcs, str);
		print(str);
		
		// Queue initial transfer - Normal TRB with IOC flag
		// TRB Type=1 (Normal), IOC=1 (bit 5), TRB Transfer Length in bits 17-31 of status field
		volatile TRB *trb1 = &kbd_device.kbd_ring.trb[kbd_device.kbd_ring.enq];
		
		print((char*)"Setting TRB fields...");
		trb1->parameter = kbd_buffer1_phys;
		print((char*)"Set parameter");
		trb1->status = 8;  // Transfer length = 8 bytes
		print((char*)"Set status");
		trb1->control = (1u << 10) | (1u << 5) | kbd_device.kbd_ring.pcs;  // Normal TRB, IOC, PCS
		print((char*)"Set control");
		
		if (++kbd_device.kbd_ring.enq == 255) {
			kbd_device.kbd_ring.enq = 0;
			kbd_device.kbd_ring.pcs ^= 1;
		}
		
		print((char*)"Transfer queued, about to ring doorbell...");
		print((char*)"Slot ID:");
		to_str(slot_id, str);
		print(str);
		print((char*)"Doorbell address:");
		to_hex((uint64_t)&doorbell32[slot_id], str);
		print(str);
		
		doorbell32[slot_id] = 3;  // Ring EP1 IN (DCI=3)
		
		print((char*)"Doorbell rung, continuing...");
		
		kbd_device.slot_id = slot_id;
		kbd_device.port_num = i + 1;
		kbd_device.speed = speed;
		kbd_device.ep0_ring = ep0;
		kbd_device.device_ctx = dev_ctx;
		device_found = true;
		
		print((char*)"Keyboard initialized!");
		print((char*)"Breaking from port scan loop...");
		break;
	}
	
	print((char*)"Exited port scan loop");
	
	if (!device_found) {
		print((char*)"No keyboard found!");
		hcf();
	}
	
	print((char*)"About to enter main loop");
	
	print((char*)"");
	print((char*)"Press any key...");
	print((char*)"");
	print((char*)"Polling for events...");
	
	// Main keyboard polling loop
	uint8_t last_keys[6] = {0};
	uint32_t event_count = 0;
	uint32_t transfer_count = 0;
	uint64_t loop_count = 0;
	
	while (true) {
		loop_count++;
		
		// Show we're still alive every million loops
		if (loop_count % 10000000 == 0) {
			print((char*)"Still polling... Events:");
			to_str(event_count, str);
			print(str);
			print((char*)"ERDP index:");
			to_str(erdp_index, str);
			print(str);
			print((char*)"CCS:");
			to_str(ccs, str);
			print(str);
			
			// Check current event ring entry
			TRB* cur_evt = (TRB*)&er_virt[erdp_index];
			print((char*)"Current TRB cycle:");
			to_str(cur_evt->control & 1, str);
			print(str);
		}
		
		TRB* evt = (TRB*)&er_virt[erdp_index];
		uint32_t ctrl = evt->control;
		uint32_t cyc = ctrl & 1u;
		
		if (cyc != ccs) {
			continue;
		}
		
		event_count++;
		if (event_count <= 10) {
			print((char*)"Event received! Type:");
			to_str((ctrl >> 10) & 0x3F, str);
			print(str);
		}
		
		uint32_t type = (ctrl >> 10) & 0x3F;
		
		if (type == 32) {  // Transfer Event
			transfer_count++;
			
			uint32_t code = (evt->status >> 24) & 0xFF;
			uint64_t trb_ptr = evt->parameter;
			uint32_t transfer_len = evt->status & 0xFFFFFF;
			
			print((char*)"Transfer event! Code:");
			to_str(code, str);
			print(str);
			print((char*)"Residual length:");
			to_str(transfer_len, str);
			print(str);
			
			if (code == 1 || code == 13) {  // Success or Short Packet
				// Get the physical address from the TRB pointer
				uint64_t report_phys = trb_ptr;
				volatile uint8_t* report = (volatile uint8_t*)(HHDM + report_phys);
				
				// Debug: show raw report data
				print((char*)"Report bytes:");
				for (int b = 0; b < 8; b++) {
					char hex_str[4];
					uint8_t val = report[b];
					hex_str[0] = HEX_NUMS[(val >> 4) & 0xF];
					hex_str[1] = HEX_NUMS[val & 0xF];
					hex_str[2] = ' ';
					hex_str[3] = '\0';
					print(hex_str);
				}
				
				// Parse HID keyboard report
				uint8_t modifiers = report[0];
				bool shift = (modifiers & 0x22) != 0;
				
				// Check for new key presses
				bool any_key = false;
				for (int k = 2; k < 8; k++) {
					uint8_t key = report[k];
					if (key == 0) continue;
					
					any_key = true;
					
					// Check if this is a new key (not in last_keys)
					bool is_new = true;
					for (int j = 0; j < 6; j++) {
						if (last_keys[j] == key) {
							is_new = false;
							break;
						}
					}
					
					if (is_new && key < 256) {
						print((char*)"New key code:");
						to_str(key, str);
						print(str);
						
						char c = shift ? hid_to_ascii_shift[key] : hid_to_ascii[key];
						if (c != 0) {
							print((char*)"Key pressed:");
							char msg[2];
							msg[0] = c;
							msg[1] = '\0';
							print(msg);
						}
					}
				}
				
				// Update last_keys
				for (int k = 0; k < 6; k++) {
					last_keys[k] = report[k + 2];
				}
				
				// Queue another transfer using the same buffer
				volatile TRB *new_trb = &kbd_device.kbd_ring.trb[kbd_device.kbd_ring.enq];
				new_trb->parameter = report_phys;
				new_trb->status = 8;
				new_trb->control = (1u << 10) | (1u << 5) | kbd_device.kbd_ring.pcs;
				
				if (++kbd_device.kbd_ring.enq == 255) {
					kbd_device.kbd_ring.enq = 0;
					kbd_device.kbd_ring.pcs ^= 1;
				}
				
				doorbell32[kbd_device.slot_id] = 3;
			} else {
				print((char*)"Transfer error, not requeueing");
			}
			
			erdp_index = (erdp_index + 1) % 256;
			if (erdp_index == 0) ccs ^= 1;
			uint64_t new_erdp = er_phys + (erdp_index * 16);
			*erdp = new_erdp | (1ull << 3);
		} else {
			// Other event types
			if (event_count <= 10) {
				print((char*)"Other event type:");
				to_str(type, str);
				print(str);
			}
			
			erdp_index = (erdp_index + 1) % 256;
			if (erdp_index == 0) ccs ^= 1;
			uint64_t new_erdp = er_phys + (erdp_index * 16);
			*erdp = new_erdp | (1ull << 3);
		}
	}
	
	print((char*)"Finished.");
	hcf();
}