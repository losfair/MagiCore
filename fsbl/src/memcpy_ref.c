// riscv64-unknown-elf-gcc -O2 -march=rv32im -mabi=ilp32 -ffreestanding -nostdlib -funroll-all-loops -S ./memcpy_ref.c 
void memcpy_u32(unsigned int *dst, unsigned int *src, int n) {
  int i;
  for (i = 0; i < n; i++) {
    dst[i] = src[i];
  }
}
