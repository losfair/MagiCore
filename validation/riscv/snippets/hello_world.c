#include <stdint.h>
#include "lib/printf.h"

volatile int *debug_print_port = (int *) 0xfe000000;
volatile int *stop_port = (int *) 0xfe000004;
volatile void *datamem = (void *) 0x20000000;

void puts(const char *s) {
  while(*s) {
    *debug_print_port = *(s++);
  }
}

void __attribute__((noinline, noreturn)) stop() {
  *stop_port = 1;
  while(1);
}

int add_it(int a, int b) {
  return a + b;
}

typedef int (*add_it_type)(int a, int b);

int app_main() {
  volatile uint32_t *a = (uint32_t *) datamem;
  volatile uint32_t *b = (uint32_t *) (datamem + 4);

  *a = 42;
  *b = 11;
  uint32_t out = *a * *b;
  printf("hello world %u function address %p\n", out, app_main);

  *a = (uint32_t) add_it;
  add_it_type f = (void *) *a;
  printf("add_it address %p output %d\n", (void *) f, f(1, 2));

  volatile uint32_t *does_not_change = (uint32_t *) 0xfe000008;
  *does_not_change = 42;
  uint32_t readback_v1 = *does_not_change;
  printf("does_not_change readback v1: %u\n", readback_v1);
  *does_not_change = 43;
  uint32_t readback_v2 = *does_not_change;
  asm volatile ("fence");
  printf("does_not_change readback v2: %u\n", readback_v2);

  stop();
}
