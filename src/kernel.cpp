#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include "image_data.h"

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

__attribute__((used, section(".limine_requests_start")))
static volatile LIMINE_REQUESTS_START_MARKER;
__attribute__((used, section(".limine_requests_end")))
static volatile LIMINE_REQUESTS_END_MARKER;


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

static inline uint64_t rdtsc() {
	uint32_t lo, hi;
	__asm__ volatile ("rdtsc" : "=a"(lo), "=d"(hi));
	return ((uint64_t)hi << 32) | lo;
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

extern "C" void kmain(void) {
	if (!LIMINE_BASE_REVISION_SUPPORTED) hcf();
	if (!fb_req.response || fb_req.response->framebuffer_count < 1) hcf();

	struct limine_framebuffer *fb = fb_req.response->framebuffers[0];
	
//	test code (gradient)
	for (uint32_t cx = 0; cx < fb->width; cx++) {
		for (uint32_t cy = 0; cy < fb->height; cy++) {
			uint8_t r = (cx*255)/fb->width;
			uint8_t g = (cy*255)/fb->width;
			putp(fb, cx, cy, toARGB(255, r, g, 0));
		}
	}
	
	auto white = toARGB(255, 255, 255, 255);
	auto black = toARGB(255, 0, 0, 0);
	
//	while (true) {
	auto w = fb->width;
	auto h = fb->height;
	
	for (uint32_t cy = 0; cy < h; cy++) {
		uint64_t y = (cy*IMG_H)/h;
		
		for (uint32_t cx = 0; cx < w; cx++) {
			uint64_t x = (cx*IMG_W)/w;
			
			uint64_t fullindx = (y*IMG_W+x);
			uint64_t valindx = fullindx/64;
			uint64_t specificindx = 63-fullindx-valindx*64;
			
			uint64_t val = img[valindx];
			bool iswhite = ((val >> specificindx) & 1) == 1;
			
			if (iswhite) {
				putp(fb, cx, cy, white);
			}else{
				putp(fb, cx, cy, black);
			}
		}
	}
//	}
	
	
//	uint8_t red = 0;
//	uint8_t green = 0;
//	uint8_t blue = 0;
//	
//	KeyState ks{};
//	fill_screen_fast(fb, toARGB(255, red, green, blue));
	
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
//		
////		for (uint32_t cx = 0; cx < fb->width; cx++) {
////			for (uint32_t cy = 0; cy < fb->height; cy++) {
////				putp(fb, cx, cy, toARGB(255, red, 0, 0));
////			}
////		}
//	}
	
	
	hcf();
}