#![no_std]
#![no_main]

mod reg;
mod sync;
mod uart;

use core::arch::asm;
use core::fmt::Write;
use core::panic::PanicInfo;

use reg::CLINT_TIME;
use riscv::register::mstatus::MPP;
use sync::io_read;

use crate::uart::UartPort;

core::arch::global_asm!(include_str!("entry.asm"));

extern "C" {
  fn do_ecall(a0: usize, a1: usize, a2: usize, a3: usize) -> usize;
}

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
  writeln!(UartPort, "MagiCore FSBL. t={}", unsafe {
    io_read(CLINT_TIME)
  })
  .unwrap();
  unsafe {
    do_ecall(42, 0, 0, 0);
  }
  writeln!(UartPort, "Exception OK.").unwrap();
  writeln!(UartPort, "Jumping to user code.").unwrap();
  unsafe {
    riscv::register::mstatus::set_mpp(MPP::User);
    riscv::register::mstatus::set_mpie();
    riscv::register::mepc::write(0x80000000);
    asm!("mret");
    loop {}
  }
}

#[panic_handler]
fn on_panic(_info: &PanicInfo) -> ! {
  loop {}
}

#[no_mangle]
pub unsafe extern "C" fn rust_intr_entry(ctx: &mut IntrContext) {
  let mut mepc: usize;
  let mcause: u32;
  let mtval: u32;
  asm!("csrr {mepc}, mepc", mepc = out(reg) mepc);
  asm!("csrr {mcause}, mcause", mcause = out(reg) mcause);
  asm!("csrr {mtval}, mtval", mtval = out(reg) mtval);

  match mcause {
    8 => {
      // ECALL from U mode
      handle_ecall(ctx);
      riscv::register::mepc::write(mepc + 4);
    }
    11 => {
      // ECALL from M mode
      writeln!(UartPort, "ECALL from M").unwrap();
      riscv::register::mepc::write(mepc + 4);
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

#[repr(C)]
pub struct BranchStats {
  pub brmiss: u64,
  pub brhit: u64,
}

unsafe fn handle_ecall(ctx: &mut IntrContext) {
  let code = ctx.read_reg(10);
  match code {
    0 => {
      // PUTCHAR
      uart::write_byte(ctx.read_reg(11) as u8);
    }
    1 => {
      // RDCYCLE
      let out: *mut u64 = ctx.read_reg(11) as *mut u64;
      let value: u32;
      let valueh: u32;
      asm!("rdcycle {out}", out = out(reg) value);
      asm!("rdcycleh {out}", out = out(reg) valueh);
      *out = ((valueh as u64) << 32) | (value as u64);
    }
    2 => {
      // RDINSTRET
      let out: *mut u64 = ctx.read_reg(11) as *mut u64;
      let value: u32;
      let valueh: u32;
      asm!("rdinstret {out}", out = out(reg) value);
      asm!("rdinstreth {out}", out = out(reg) valueh);
      *out = ((valueh as u64) << 32) | (value as u64);
    }
    3 => {
      // RD BRINFO
      let out = &mut *(ctx.read_reg(11) as *mut BranchStats);
      let brmiss: u32;
      let brmissh: u32;
      asm!("csrr {out}, 0xc03", out = out(reg) brmiss);
      asm!("csrr {out}, 0xc83", out = out(reg) brmissh);
      out.brmiss = ((brmissh as u64) << 32) | (brmiss as u64);
      let brhit: u32;
      let brhith: u32;
      asm!("csrr {out}, 0xc04", out = out(reg) brhit);
      asm!("csrr {out}, 0xc84", out = out(reg) brhith);
      out.brhit = ((brhith as u64) << 32) | (brhit as u64);
    }
    _ => {
      writeln!(UartPort, "Unknown ecall: {}", code).unwrap();
    }
  }
}
