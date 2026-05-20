typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long uint64_t;
typedef long int64_t;

// Minimal syscall wrappers for window management
static inline uint64_t sys_win_create(const char* title, uint64_t x, uint64_t y, uint64_t w, uint64_t h, uint64_t flags) {
    uint64_t ret;
    __asm__ volatile (
        "movq %5, %%r10\n"
        "movq %6, %%r8\n"
        "syscall"
        : "=a"(ret)
        : "a"(200), "D"(title), "S"(x), "d"(y), "r"(w), "r"(h)
        : "rcx", "r11", "r10", "r8", "memory"
    );
    return ret;
}

static inline void sys_win_sync(uint64_t id) {
    __asm__ volatile ("syscall" : : "a"(201), "D"(id) : "rcx", "r11", "memory");
}

static inline uint64_t sys_win_get_buffer(uint64_t id) {
    uint64_t ret;
    __asm__ volatile ("syscall" : "=a"(ret) : "a"(203), "D"(id) : "rcx", "r11", "memory");
    return ret;
}

static inline void sys_write(uint64_t fd, const char* s, uint64_t len) {
    __asm__ volatile ("syscall" : : "a"(1), "D"(fd), "S"(s), "d"(len) : "rcx", "r11", "memory");
}

void print(const char* s) {
    int len = 0; while(s[len]) len++;
    sys_write(1, s, len);
}

void _start() {
    print("Window Test Application Starting...\n");
    
    uint64_t win = sys_win_create("User Window!", 400, 250, 600, 400, 1);
    if (!win) {
        print("Failed to create window!\n");
        return;
    }
    
    print("Window created successfully.\n");
    
    uint32_t* buf = (uint32_t*)sys_win_get_buffer(win);
    if (!buf) {
        print("Failed to get window buffer!\n");
        return;
    }
    
    // Draw a gradient or pattern
    for (uint32_t y = 0; y < 400; y++) {
        for (uint32_t x = 0; x < 600; x++) {
            uint8_t r = (uint8_t)(x * 255 / 600);
            uint8_t g = (uint8_t)(y * 255 / 400);
            uint8_t b = 128;
            buf[y * 600 + x] = (0xFF << 24) | (r << 16) | (g << 8) | b;
        }
    }
    
    sys_win_sync(win);
    print("Window buffer synced.\n");
    
    // Animation loop
    uint32_t frame = 0;
    while (1) {
        frame++;
        for (uint32_t x = 0; x < 600; x++) {
            buf[x] = (frame + x) % 2 ? 0xFFFFFFFF : 0xFF000000;
        }
        sys_win_sync(win);
        // Simple delay loop
        for (volatile int i = 0; i < 1000000; i++);
    }
}
