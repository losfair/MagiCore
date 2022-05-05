#![no_std]
#![no_main]

mod reg;
mod sync;
mod uart;

use core::arch::asm;
use core::fmt::Write;
use core::panic::PanicInfo;
use core::sync::atomic::{AtomicU32, Ordering};

use crate::uart::UartPort;
use riscv::register::mstatus::MPP;

#[cfg(target_arch = "riscv64")]
core::arch::global_asm!(include_str!("entry_64.asm"));

#[cfg(target_arch = "riscv32")]
core::arch::global_asm!(include_str!("entry_32.asm"));

extern "C" {
  fn test_amo_add_w(ptr: &AtomicU32) -> u32;
  fn test_amo_swap_w(ptr: &AtomicU32) -> u32;
  fn test_amo_max_w(ptr: &AtomicU32) -> u32;
}

#[no_mangle]
pub unsafe extern "C" fn rust_main() -> ! {
  #[cfg(target_arch = "riscv64")]
  const ARCH_NAME: &str = "rv64";
  #[cfg(target_arch = "riscv32")]
  const ARCH_NAME: &str = "rv32";

  writeln!(UartPort, "MagiCore FSBL ({})", ARCH_NAME).unwrap();

  self_test();
  writeln!(UartPort, "Self test OK.").unwrap();

  riscv::register::mstatus::set_mpp(MPP::Machine);
  riscv::register::mepc::write(0x60000000);
  asm!("mret");
  loop {}
}

unsafe fn self_test() {
  let atomic_u32 = AtomicU32::new(0);
  assert_eq!(test_amo_add_w(&atomic_u32), 0);
  assert_eq!(test_amo_add_w(&atomic_u32), 1);
  assert_eq!(test_amo_swap_w(&atomic_u32), 2);
  assert_eq!(atomic_u32.load(Ordering::Relaxed), 35);
  assert_eq!(test_amo_max_w(&atomic_u32), 35);
  assert_eq!(test_amo_max_w(&atomic_u32), 35);
  atomic_u32.store(10, Ordering::Relaxed);
  assert_eq!(test_amo_max_w(&atomic_u32), 10);
  assert_eq!(atomic_u32.load(Ordering::Relaxed), 22);
}

#[panic_handler]
fn on_panic(_info: &PanicInfo) -> ! {
  writeln!(UartPort, "PANIC").unwrap();
  loop {}
}
