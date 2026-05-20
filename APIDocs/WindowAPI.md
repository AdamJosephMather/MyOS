# MyOS Window Management API

This document describes the dynamic Window Management API introduced in the 2.0 refactor. Programs can now create and manage windows via syscalls without kernel-level modifications.

## Syscall Overview

| Syscall | Action | Arguments | Returns |
| :--- | :--- | :--- | :--- |
| **`SYS_WIN_CREATE`** | Creates a new window | `title` (str), `x`, `y`, `w`, `h` | `window_id` (int) |
| **`SYS_WIN_GET_BUFFER`**| Retrieves pixel buffer | `window_id` | `buffer_ptr` (uint32_t*) |
| **`SYS_WIN_SYNC`** | Commits buffer to screen | `window_id` | `0` on success |
| **`SYS_WIN_DESTROY`** | Closes and frees window | `window_id` | `0` on success |

---

## Technical Details

### Pixel Format
The window buffer uses **32-bit ARGB8888** format.
- `0xFFFF0000`: Solid Red
- `0xFF00FF00`: Solid Green
- `0xFF0000FF`: Solid Blue
- `0xFF000000`: Black
- `0xFFFFFFFF`: White

### Lifecycle
1. **Creation**: When you call `SYS_WIN_CREATE`, the system allocates a `Window` struct and a backbuffer of size `w * h * 4` bytes.
2. **Buffer Access**: `SYS_WIN_GET_BUFFER` returns the linear address of this buffer. You should not free this pointer yourself.
3. **Synchronization**: The compositor runs at 60Hz. Calling `SYS_WIN_SYNC` marks your window as "dirty," prompting the compositor to blit your buffer to VRAM in the next pass.
4. **Destruction**: `SYS_WIN_DESTROY` removes the window from the Z-stack, frees the backbuffer, and releases the `Window` struct.

---

## Usage Example (C)

```c
#include "syscall.h"

void main() {
    // 1. Create a 400x300 window
    int win_id = syscall(SYS_WIN_CREATE, "My App", 100, 100, 400, 300);

    // 2. Get the drawing surface
    uint32_t* buffer = (uint32_t*)syscall(SYS_WIN_GET_BUFFER, win_id);

    // 3. Draw something (e.g., fill with blue)
    for (int i = 0; i < 400 * 300; i++) {
        buffer[i] = 0xFF0000FF; 
    }

    // 4. Update the screen
    syscall(SYS_WIN_SYNC, win_id);

    // 5. App loop...
    while(1) {
        // Handle logic
    }
}
```

---

## System Windows
Initial system windows (Terminal, Boot Log, etc.) are created using the same internal API but with `is_system=true` flags, which allow for special rendering logic (like the terminal text matrix). User applications should generally use `WIN_TYPE_USER`.
