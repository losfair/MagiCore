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

core::arch::global_asm!(include_str!("entry.asm"));

extern "C" {
  fn do_ecall(a0: usize, a1: usize, a2: usize, a3: usize) -> usize;
  fn test_amo_add_w(ptr: &AtomicU32) -> u32;
  fn test_amo_swap_w(ptr: &AtomicU32) -> u32;
  fn test_amo_max_w(ptr: &AtomicU32) -> u32;
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
  do_ecall(42, 0, 0, 0);
}

#[panic_handler]
fn on_panic(_info: &PanicInfo) -> ! {
  writeln!(UartPort, "PANIC").unwrap();
  loop {}
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
    writeln!(UartPort, "Unhandled interrupt: {}", code).unwrap();
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
