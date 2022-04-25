use core::fmt::Write;

use crate::sync::{io_read, io_write};

const UART_RW_PORT: *mut u32 = 0xff010000 as *mut u32;
const UART_RW_CAPACITY: *mut u32 = 0xff010004 as *mut u32;

pub fn write_byte(x: u8) {
  unsafe {
    while io_read(UART_RW_CAPACITY) == 0 {}
    io_write(UART_RW_PORT, x as u32);
  }
}

pub fn read_byte() -> Option<u8> {
  unsafe {
    if io_read(UART_RW_CAPACITY) == 0 {
      None
    } else {
      Some(io_read(UART_RW_PORT) as u8)
    }
  }
}

pub struct UartPort;

impl Write for UartPort {
  fn write_str(&mut self, s: &str) -> core::fmt::Result {
    let bytes = s.as_bytes();
    for b in bytes {
      write_byte(*b);
    }
    Ok(())
  }
}
