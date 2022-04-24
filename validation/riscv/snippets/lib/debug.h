#pragma once

static inline void _putchar(char c) {
  * (volatile int *) 0xfe000000 = c;
}
