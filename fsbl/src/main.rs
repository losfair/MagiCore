#![no_std]
#![no_main]

mod sync;
mod uart;

use core::fmt::Write;
use core::panic::PanicInfo;

use crate::uart::UartPort;

core::arch::global_asm!(include_str!("entry.asm"));

extern "C" {
  fn do_ecall(a0: usize, a1: usize, a2: usize, a3: usize) -> usize;
}

static mut DUMMY_MEM: u32 = 0;

#[repr(C)]
pub struct IntrContext {
  pub regs: [u32; 31],
}

impl IntrContext {
  fn read_reg(&self, reg: usize) -> u32 {
    if reg == 0 {
      0
    } else {
      let index = reg - 1;
      assert!(index < self.regs.len());
      self.regs[index]
    }
  }
}

#[no_mangle]
pub extern "C" fn rust_main() -> ! {
  loop {
    let cyc: u32;
    let cyc_h: u32;
    let instret: u32;
    let instret_h: u32;
    unsafe {
      core::arch::asm!("csrr {cyc}, cycle", cyc = out(reg) cyc);
      core::arch::asm!("csrr {cyc}, cycleh", cyc = out(reg) cyc_h);
      core::arch::asm!("csrr {instret}, instret", instret = out(reg) instret);
      core::arch::asm!("csrr {instret}, instreth", instret = out(reg) instret_h);
    }
    let cycles = (cyc as u64) | ((cyc_h as u64) << 32);
    let instret = (instret as u64) | ((instret_h as u64) << 32);
    writeln!(
      UartPort,
      "MiniRV32 FSBL - cycles={} instret={}",
      cycles, instret
    )
    .unwrap();
    writeln!(UartPort, "Throwing exception").unwrap();
    unsafe {
      do_ecall(42, 0, 0, 0);
    }
    writeln!(UartPort, "Returning from exception").unwrap();
    writeln!(UartPort, "Jumping to user code").unwrap();
    unsafe {
      let code = 0x80000000 as *mut u8;
      let code = core::mem::transmute::<*mut u8, unsafe extern "C" fn()>(code);
      code();
    }

    for _ in 0..1000 {
      unsafe {
        core::ptr::write_volatile(&mut DUMMY_MEM, 0);
      }
    }
  }
}

#[panic_handler]
fn on_panic(_info: &PanicInfo) -> ! {
  loop {}
}

#[no_mangle]
pub unsafe extern "C" fn rust_intr_entry(ctx: &mut IntrContext) {
  let mut mepc: u32;
  let mcause: u32;
  let mtval: u32;
  core::arch::asm!("csrr {mepc}, mepc", mepc = out(reg) mepc);
  core::arch::asm!("csrr {mcause}, mcause", mcause = out(reg) mcause);
  core::arch::asm!("csrr {mtval}, mtval", mtval = out(reg) mtval);

  match mcause {
    11 => {
      // ECALL from M
      handle_ecall(ctx);
      mepc += 4;
      core::arch::asm!("csrw mepc, {mepc}", mepc = in(reg) mepc);
    }
    _ => {
      writeln!(
        UartPort,
        "Unknown exception: PC {} cause {} mtval {} - lockup.",
        mepc, mcause, mtval
      )
      .unwrap();
      loop {}
    }
  }
}

fn handle_ecall(ctx: &mut IntrContext) {
  let code = ctx.read_reg(10);
  match code {
    0 => {
      // PUTCHAR
      uart::write_byte(ctx.read_reg(11) as u8);
    }
    _ => {
      writeln!(UartPort, "Unknown ecall: {}", code).unwrap();
    }
  }
}
