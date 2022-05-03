#![allow(dead_code)]

pub const CLINT_BASE: usize = 0xff020000;
pub const CLINT_CMP: *mut u32 = (CLINT_BASE + 0x4000) as *mut u32;
pub const CLINT_TIME: *mut u32 = (CLINT_BASE + 0xBFF8) as *mut u32;

pub const PLIC_BASE: usize = 0xff010200;
pub const PLIC_PENDINGS: *mut u32 = (PLIC_BASE + 0x0) as *mut u32;
pub const PLIC_MASKS: *mut u32 = (PLIC_BASE + 0x4) as *mut u32;

pub const MAS_BASE: usize = 0xff010100;
pub const MAS_BUFFER_PTR: *mut u32 = (MAS_BASE + 0x0) as *mut u32;
pub const MAS_SOFT_ENABLE: *mut u32 = (MAS_BASE + 0x4) as *mut u32;
pub const MAS_SIZE: usize = 262144;

pub const MAS_DATA: *mut u32 = 0xff800000 as *mut u32;

pub const MAS_INTERRUPT_ID: usize = 0x1;
