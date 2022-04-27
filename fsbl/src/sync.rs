use core::arch::asm;

pub unsafe fn io_write<T: Copy>(memory: *mut T, value: T) {
  core::ptr::write_volatile(memory, value);
  asm!("fence w, rw");
}

// Usually we don't need to fence I/O reads on MagiCore because
// transactions issued to the same device are completed in the original order
pub unsafe fn io_read<T: Copy>(memory: *const T) -> T {
  core::ptr::read_volatile(memory)
}
