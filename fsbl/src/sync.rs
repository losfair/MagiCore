use core::arch::asm;

pub unsafe fn io_write<T: Copy>(memory: *mut T, value: T) {
  core::ptr::write_volatile(memory, value);
  asm!("fence rw, w");
}

pub unsafe fn io_read<T: Copy>(memory: *const T) -> T {
  core::ptr::read_volatile(memory)
}
