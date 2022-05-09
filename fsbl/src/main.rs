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

extern "C" {
  fn ebpf_test();
}

#[no_mangle]
pub unsafe extern "C" fn rust_main() -> ! {
  #[cfg(target_arch = "riscv64")]
  const ARCH_NAME: &str = "rv64";
  #[cfg(target_arch = "riscv32")]
  const ARCH_NAME: &str = "rv32";

  writeln!(UartPort, "MagiCore-wBPF FSBL ({})", ARCH_NAME).unwrap();

  asm!("csrw 0x7c1, {x}", x = in(reg) 1); // mdecodemode

  riscv::register::mstatus::set_mpp(MPP::User);
  riscv::register::mepc::write(ebpf_test as usize);
  asm!("mret");
  loop {}
}

#[panic_handler]
fn on_panic(_info: &PanicInfo) -> ! {
  writeln!(UartPort, "PANIC").unwrap();
  loop {}
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
