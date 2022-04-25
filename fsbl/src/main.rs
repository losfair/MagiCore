#![no_std]
#![no_main]

mod sync;
mod uart;

use core::fmt::Write;
use core::panic::PanicInfo;

use sync::io_write;

use crate::uart::UartPort;

core::arch::global_asm!(include_str!("entry.asm"));

static mut DUMMY_MEM: u32 = 0;

#[repr(C)]
pub struct IntrContext {
  pub regs: [u32; 31],
}

#[no_mangle]
pub extern "C" fn rust_main() -> ! {
  loop {
    let cyc: u32;
    let cyc_h: u32;
    unsafe {
      core::arch::asm!("csrr {cyc}, cycle", cyc = out(reg) cyc);
      core::arch::asm!("csrr {cyc}, cycleh", cyc = out(reg) cyc_h);
    }
    let cycles = (cyc as u64) | ((cyc_h as u64) << 32);
    writeln!(UartPort, "{}", cycles).unwrap();
    writeln!(UartPort, "Throwing exception").unwrap();
    unsafe {
      core::arch::asm!("ecall");
    }
    writeln!(UartPort, "Returning from exception").unwrap();

    for i in 0..100000 {
      unsafe {
        io_write(&mut DUMMY_MEM, 0);
      }
    }
  }
}

#[panic_handler]
fn on_panic(_info: &PanicInfo) -> ! {
  loop {}
}

#[no_mangle]
pub extern "C" fn rust_intr_entry(_ctx: &mut IntrContext) {
  let mepc: u32;
  unsafe {
    core::arch::asm!("csrr {mepc}, mepc", mepc = out(reg) mepc);
  }
  writeln!(UartPort, "P = {}", mepc).unwrap();
}
