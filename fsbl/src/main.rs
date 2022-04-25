#![no_std]
#![no_main]

mod sync;
mod uart;

use core::fmt::Write;
use core::panic::PanicInfo;

use crate::uart::UartPort;

core::arch::global_asm!(include_str!("entry.asm"));

#[no_mangle]
pub extern "C" fn rust_main() -> ! {
  writeln!(UartPort, "MiniRV32 FSBL").unwrap();
  loop {}
}

#[panic_handler]
fn on_panic(_info: &PanicInfo) -> ! {
  loop {}
}
