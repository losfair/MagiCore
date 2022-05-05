#![no_std]
#![no_main]

mod reg;
mod sync;
mod uart;

use core::arch::asm;
use core::fmt::Write;
use core::panic::PanicInfo;

use crate::uart::UartPort;
use riscv::register::mstatus::MPP;

#[cfg(target_arch = "riscv64")]
core::arch::global_asm!(include_str!("entry_64.asm"));

#[cfg(target_arch = "riscv32")]
core::arch::global_asm!(include_str!("entry_32.asm"));

#[no_mangle]
pub unsafe extern "C" fn rust_main() -> ! {
  #[cfg(target_arch = "riscv64")]
  const ARCH_NAME: &str = "rv64";
  #[cfg(target_arch = "riscv32")]
  const ARCH_NAME: &str = "rv32";

  writeln!(UartPort, "MagiCore FSBL ({})", ARCH_NAME).unwrap();

  riscv::register::mstatus::set_mpp(MPP::Machine);
  riscv::register::mepc::write(0x60000000);
  asm!("mret");
  loop {}
}

#[panic_handler]
fn on_panic(_info: &PanicInfo) -> ! {
  writeln!(UartPort, "PANIC").unwrap();
  loop {}
}
