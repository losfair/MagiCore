#!/bin/bash

set -e

riscv64-unknown-elf-gcc -march=rv32im -mabi=ilp32 \
    -nostdlib -ffreestanding -O0 \
    -Wl,-T -Wl,./linker.ld \
    -o "$1.elf" "$1.c" "entry.S" "lib/printf.c" "lib/softdivide.c" "lib/polyfill.c"

llvm-objcopy \
  "$1.elf" \
  --binary-architecture=riscv32 --strip-all -O binary \
  --only-section=.text \
  --only-section=.data \
  "$1.bin"

llvm-objdump --mattr=+m -D "$1.elf" > "$1.dump"
hexdump -C "$1.bin" > "$1.hex"
