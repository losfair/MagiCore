#![no_std]
#![no_main]

mod reg;
mod sync;
mod uart;

use core::arch::asm;
use core::fmt::Write;
use core::panic::PanicInfo;

use reg::{INTC_PENDINGS, MAS_DATA};
use riscv::register::mstatus::MPP;
use sync::io_read;

use crate::reg::{INTC_MASKS, MAS_BUFFER_PTR, MAS_SOFT_ENABLE};
use crate::sync::io_write;
use crate::uart::UartPort;

core::arch::global_asm!(include_str!("entry.asm"));
core::arch::global_asm!(include_str!("memcpy.asm"));

extern "C" {
  fn do_ecall(a0: usize, a1: usize, a2: usize, a3: usize) -> usize;
  fn memcpy_u32(dst: *mut u32, src: *mut u32, n: usize);
}

#[repr(C)]
pub struct IntrContext {
  pub regs: [u64; 31],
}

impl IntrContext {
  fn read_reg(&self, reg: usize) -> u64 {
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
pub unsafe extern "C" fn rust_main() -> ! {
  writeln!(UartPort, "MagiCore FSBL").unwrap();
  do_ecall(42, 0, 0, 0);

  // Enable MAS
  /*riscv::register::mie::set_mext();
  io_write(INTC_MASKS, 1u32 << 16); // MAS interrupt
  io_write(MAS_SOFT_ENABLE, 1);
  writeln!(UartPort, "MAS enabled. Jumping to user code.").unwrap();*/
  writeln!(UartPort, "MAS not enabled.").unwrap();

  // Jump to user program
  //riscv::register::mstatus::set_mpp(MPP::User);
  riscv::register::mstatus::set_mpp(MPP::Machine);
  //riscv::register::mstatus::set_mpie();
  riscv::register::mepc::write(0x60000000);
  asm!("mret");
  loop {}
}

#[panic_handler]
fn on_panic(_info: &PanicInfo) -> ! {
  loop {}
}

unsafe fn handle_ext_interrupt(_ctx: &mut IntrContext) {
  // Query INTC
  let pending = io_read(INTC_PENDINGS);
  if pending & (1 << 16) != 0 {
    // MAS interrupt
    let count = io_read(MAS_BUFFER_PTR) as usize;
    writeln!(UartPort, "Starting MAS copy of {} samples.", count).unwrap();
    let start = riscv::register::cycle::read();
    core::ptr::copy(MAS_DATA as *mut u64, 0x60800000 as *mut u64, count / 2);
    //memcpy_u32(0x60800000 as *mut u32, MAS_DATA, count);
    let end = riscv::register::cycle::read();
    writeln!(
      UartPort,
      "Memory copy of {} bytes took {} cycles",
      count * 4,
      end - start
    )
    .unwrap();
    io_write(MAS_BUFFER_PTR, 0);
    io_write(INTC_PENDINGS, 1u32 << 16);
  }
}

#[no_mangle]
pub unsafe extern "C" fn rust_intr_entry(ctx: &mut IntrContext) {
  let mut mepc: usize;
  let mcause: usize;
  let mtval: usize;
  asm!("csrr {mepc}, mepc", mepc = out(reg) mepc);
  asm!("csrr {mcause}, mcause", mcause = out(reg) mcause);
  asm!("csrr {mtval}, mtval", mtval = out(reg) mtval);

  let is_intr = (mcause as isize) < 0;
  let code = (mcause << 1) >> 1;
  if is_intr {
    match code {
      0xb => {
        handle_ext_interrupt(ctx);
      }
      _ => {
        writeln!(UartPort, "Unhandled interrupt: {}", code).unwrap();
      }
    }
  } else {
    match code {
      8 | 11 => {
        // ECALL from U (8) or M (11) mode
        handle_ecall(ctx);
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
      let value: u64;
      asm!("rdcycle {out}", out = out(reg) value);
      *out = value;
    }
    2 => {
      // RDINSTRET
      let out: *mut u64 = ctx.read_reg(11) as *mut u64;
      let value: u64;
      asm!("rdinstret {out}", out = out(reg) value);
      *out = value;
    }
    3 => {
      // RD BRINFO
      let out = &mut *(ctx.read_reg(11) as *mut BranchStats);
      let brmiss: u64;
      asm!("csrr {out}, 0xc03", out = out(reg) brmiss);
      out.brmiss = brmiss;
      let brhit: u64;
      asm!("csrr {out}, 0xc04", out = out(reg) brhit);
      out.brhit = brhit;
    }
    _ => {
      writeln!(UartPort, "Unknown ecall: {}", code).unwrap();
    }
  }
}
