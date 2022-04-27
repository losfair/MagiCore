use core::arch::asm;

pub unsafe fn io_write<T: Copy>(memory: *mut T, value: T) {
  core::ptr::write_volatile(memory, value);
  asm!("fence w, rw");
}

pub unsafe fn io_read<T: Copy>(memory: *const T) -> T {
  asm!("fence");
  core::ptr::read_volatile(memory)
}
