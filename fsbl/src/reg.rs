pub const CLINT_BASE: usize = 0xff020000;
pub const CLINT_CMP: *mut u32 = (CLINT_BASE + 0x4000) as *mut u32;
pub const CLINT_TIME: *mut u32 = (CLINT_BASE + 0xBFF8) as *mut u32;
