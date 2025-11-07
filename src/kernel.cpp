#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
//#include "image_data.h"

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


uint64_t HHDM;


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

static inline void putp(struct limine_framebuffer *fb, uint32_t x, uint32_t y, uint32_t argb) {
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

static inline void fill_screen_fast(struct limine_framebuffer *fb, uint32_t argb) {
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








// Implements a simple memory set function
//void *memset(void *s, int c, size_t n) {
//	volatile uint8_t *p = (volatile uint8_t *)s; // Treat the memory block as an array of bytes
//	
//	// Loop 'n' times, setting each byte to the value 'c'
//	for (size_t i = 0; i < n; i++) {
//		p[i] = (uint8_t)c;
//	}
//	return s;
//}

//#define PAGE_SIZE 4096
//
//static inline uint64_t round_up(uint64_t x, uint64_t a)   { return (x + a - 1) & ~(a - 1); }
//static inline uint64_t round_down(uint64_t x, uint64_t a) { return x & ~(a - 1); }
//
//static void bitmap_set_range(uint8_t *bm, uint64_t first_page, uint64_t page_count, bool used) {
////	set/clear bits [first_page, first_page+page_count)
//	uint64_t p = first_page, end = first_page + page_count;
//	while (p < end) {
//		if (used) bm[p >> 3] |=  (1u << (p & 7));
//		else      bm[p >> 3] &= ~(1u << (p & 7));
//		++p;
//	}
//}
//
//void init_phys_allocator() {
//	// 1) size
//	uint64_t highest = 0;
//	for (size_t i = 0; i < memmap_req.response->entry_count; ++i) {
//		auto *e = memmap_req.response->entries[i];
//		uint64_t end = e->base + e->length;
//		if (end > highest) highest = end;
//	}
//	
//	uint64_t total_pages = round_up(highest, PAGE_SIZE) / PAGE_SIZE;
//	uint64_t bitmap_size = round_up((total_pages + 7) / 8, 8); // byte-align
//
//	// 2) place bitmap in a USABLE region
//	uint64_t bitmap_pa = 0;
//	for (size_t i = 0; i < memmap_req.response->entry_count; ++i) {
//		auto *e = memmap_req.response->entries[i];
//		if (e->type != LIMINE_MEMMAP_USABLE) continue;
//
//		uint64_t start = e->base < 0x100000 ? 0x100000 : e->base; // avoid <1MiB
//		uint64_t aligned = round_up(start, PAGE_SIZE);
//		if (aligned + bitmap_size <= e->base + e->length) {
//			bitmap_pa = aligned;
//			break;
//		}
//	}
//	if (!bitmap_pa) hcf();
//
//	// 3) VA for bitmap via HHDM
//	uint8_t *bm = (uint8_t *)(HHDM + bitmap_pa);
//
//	// 4) init: mark everything used
//	for (uint64_t i = 0; i < bitmap_size; ++i) bm[i] = 0xFF;
//
//	// 5) free USABLE ranges; reserve others
//	for (size_t i = 0; i < memmap_req.response->entry_count; ++i) {
//		auto *e = memmap_req.response->entries[i];
//
//		uint64_t base = round_down(e->base, PAGE_SIZE);
//		uint64_t end  = round_up(e->base + e->length, PAGE_SIZE);
//		uint64_t pages = (end - base) / PAGE_SIZE;
//
//		bool usable = (e->type == LIMINE_MEMMAP_USABLE);
//		bitmap_set_range(bm, base / PAGE_SIZE, pages, !usable);
//	}
//
//	// 6) explicitly reserve the bitmap itself
//	bitmap_set_range(bm, bitmap_pa / PAGE_SIZE, round_up(bitmap_size, PAGE_SIZE)/PAGE_SIZE, true);
//
//	// TODO: also reserve:
//	// - kernel image phys range
//	// - initial page tables
//	// - stacks
//	// - framebuffer (device memory)
//}

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
};

struct MCFGHeader {
	struct ACPISDTHeader header;   // the same 36-byte header you already know
	uint64_t reserved;             // always 0
	struct {
		uint64_t base_address;     // physical base of PCIe config space
		uint16_t segment_group;
		uint8_t  bus_start;
		uint8_t  bus_end;
		uint32_t reserved2;
	} entries[];                   // one or more of these
};


uint64_t pci_get_config_va(uint64_t base, uint8_t bus, uint8_t device, uint8_t function) {
	uint64_t offset = ((uint64_t)bus << 20) | ((uint64_t)device << 15) | ((uint64_t)function << 12);
	return HHDM + base + offset;
}

bool find_usb_controller(uint64_t base, uint8_t start_bus, uint8_t end_bus) {
	for (uint8_t bus = start_bus; bus <= end_bus; bus++) {
		for (uint8_t dev = 0; dev < 32; dev++) {
			for (uint8_t func = 0; func < 8; func++) {
				uint64_t cfg_va = pci_get_config_va(base, bus, dev, func);
				volatile uint8_t *cfg = (volatile uint8_t *)cfg_va;

				uint16_t vendor_id = *(volatile uint16_t *)(cfg + 0x00);
				if (vendor_id == 0xFFFF) continue;  // empty slot

				uint8_t class_code = *(volatile uint8_t *)(cfg + 0x0B);
				uint8_t subclass   = *(volatile uint8_t *)(cfg + 0x0A);

				if (class_code == 0x0C && subclass == 0x03) {
					// USB controller found
					return true;
				}

				// check if multi-function
				if (func == 0 && !(*(volatile uint8_t *)(cfg + 0x0E) & 0x80))
					break; // single-function device → skip rest
			}
		}
	}
	return false;
}

extern "C" void kmain(void) {
	auto good = toARGB(255, 0, 255, 0);
	auto bad = toARGB(255, 255, 0, 0);
	auto ehhm = toARGB(255, 0, 255, 255);
	
	// frame buffer
	if (!LIMINE_BASE_REVISION_SUPPORTED) hcf();
	if (!fb_req.response || fb_req.response->framebuffer_count < 1) hcf();
	
	struct limine_framebuffer *fb = fb_req.response->framebuffers[0];
	
	if (!memmap_req.response) {
		fill_screen_fast(fb, bad);
		hcf();
	}
	
	if (!hhdm_req.response) {
		fill_screen_fast(fb, bad);
		hcf();
	}
	
	HHDM = hhdm_req.response->offset;
	// HHDM + PA = VA
	
	
	
	// RSDP table (will point to the thing needed to find PCIe)
	if (!rsdp_req.response) hcf();
	uint64_t rsdp_va = (uint64_t)rsdp_req.response->address;
	
//	uint64_t rsdp_va = HHDM+rsdp_physical_address;
	
	
	
	
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
		fill_screen_fast(fb, bad);
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
		fill_screen_fast(fb, bad);
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
	
		// compare signature
		if (entry[0]=='M' && entry[1]=='C' && entry[2]=='F' && entry[3]=='G') {
			ptr_mcfg = entry;
			break;
		}
	}
	
	if (ptr_mcfg == 0) {
		fill_screen_fast(fb, bad);
		hcf();
	}
	
	// now we have ptr_mcfg
	
	volatile struct MCFGHeader *mcfg = (volatile struct MCFGHeader *)ptr_mcfg;
	
	uint32_t length = mcfg->header.length;
	uint32_t entries = (length - sizeof(struct ACPISDTHeader) - 8) / 16;  // 16 bytes per entry
	
	bool foundit = false;
	
	for (uint32_t i = 0; i < entries; i++) {
		auto *e = &mcfg->entries[i];
		
		if (find_usb_controller(e->base_address, e->bus_start, e->bus_end)) {
			foundit = true;
			break;
		}
	}
	
	if (!foundit) {
		fill_screen_fast(fb, bad);
		hcf();
	}
	
	
	
	fill_screen_fast(fb, good);
	
	
	
	
	
	
//	init_phys_allocator();
	
	
	
	
	
	
	
//	auto white = toARGB(255, 255, 255, 255);
//	auto black = toARGB(255, 0, 0, 0);
//	
//	while (true) {
//	auto w = fb->width;
//	auto h = fb->height;
//	
//	for (uint32_t cy = 0; cy < h; cy++) {
//		uint64_t y = (cy*IMG_H)/h;
//		
//		for (uint32_t cx = 0; cx < w; cx++) {
//			uint64_t x = (cx*IMG_W)/w;
//			
//			uint64_t fullindx = (y*IMG_W+x);
//			uint64_t valindx = fullindx/64;
//			uint64_t specificindx = 63-fullindx-valindx*64;
//			
//			uint64_t val = img[valindx];
//			bool iswhite = ((val >> specificindx) & 1) == 1;
//			
//			if (iswhite) {
//				putp(fb, cx, cy, white);
//			}else{
//				putp(fb, cx, cy, black);
//			}
//		}
//	}
//	}
	
	
//	uint8_t red = 0;
//	uint8_t green = 0;
//	uint8_t blue = 0;
//	
//	KeyState ks{};
//	fill_screen_fast(fb, toARGB(255, red, green, blue));
//	
//	while (true) {
//		if (canGetKey()) {
//			uint8_t scancode = readScancode();
//			char c = scancode_to_char(scancode, ks);
//			
//			if (c == 'r') {
//				red += 10;
//			}else if (c == 'g') {
//				green += 10;
//			}else if (c == 'b') {
//				blue += 10;
//			}else if (c == 'R') {
//				red -= 10;
//			}else if (c == 'G') {
//				green -= 10;
//			}else if (c == 'B') {
//				blue -= 10;
//			}else if (c == 'q') {
//				break;
//			}
//			
//			fill_screen_fast(fb, toARGB(255, red, green, blue));
//		}
//	}
	
	
	hcf();
}