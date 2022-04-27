.section .text.entry

.globl _start
_start:
    li x1, 0
    li x2, 0
    li x3, 0
    li x4, 0
    li x5, 0
    li x6, 0
    li x7, 0
    li x8, 0
    li x9, 0
    li x10, 0
    li x11, 0
    li x12, 0
    li x13, 0
    li x14, 0
    li x15, 0
    li x16, 0
    li x17, 0
    li x18, 0
    li x19, 0
    li x20, 0
    li x21, 0
    li x22, 0
    li x23, 0
    li x24, 0
    li x25, 0
    li x26, 0
    li x27, 0
    li x28, 0
    li x29, 0
    li x30, 0
    li x31, 0
    li sp, 0x24000

    la a0, _intr_entry
    csrw mtvec, a0
    call rust_main

_lockup:
    j _lockup

_intr_entry:
    addi sp, sp, -128
    sw x1, 0(sp)
    sw x2, 4(sp)
    sw x3, 8(sp)
    sw x4, 12(sp)
    sw x5, 16(sp)
    sw x6, 20(sp)
    sw x7, 24(sp)
    sw x8, 28(sp)
    sw x9, 32(sp)
    sw x10, 36(sp)
    sw x11, 40(sp)
    sw x12, 44(sp)
    sw x13, 48(sp)
    sw x14, 52(sp)
    sw x15, 56(sp)
    sw x16, 60(sp)
    sw x17, 64(sp)
    sw x18, 68(sp)
    sw x19, 72(sp)
    sw x20, 76(sp)
    sw x21, 80(sp)
    sw x22, 84(sp)
    sw x23, 88(sp)
    sw x24, 92(sp)
    sw x25, 96(sp)
    sw x26, 100(sp)
    sw x27, 104(sp)
    sw x28, 108(sp)
    sw x29, 112(sp)
    sw x30, 116(sp)
    sw x31, 120(sp)

    mv a0, sp

    call rust_intr_entry

    mv a0, sp

.globl asm_return_from_interrupt
asm_return_from_interrupt:
    lw x1, 0(a0)
    lw x2, 4(a0)
    lw x3, 8(a0)
    lw x4, 12(a0)
    lw x5, 16(a0)
    lw x6, 20(a0)
    lw x7, 24(a0)
    lw x8, 28(a0)
    lw x9, 32(a0)
    # lw x10, 36(a0) # a0
    lw x11, 40(a0)
    lw x12, 44(a0)
    lw x13, 48(a0)
    lw x14, 52(a0)
    lw x15, 56(a0)
    lw x16, 60(a0)
    lw x17, 64(a0)
    lw x18, 68(a0)
    lw x19, 72(a0)
    lw x20, 76(a0)
    lw x21, 80(a0)
    lw x22, 84(a0)
    lw x23, 88(a0)
    lw x24, 92(a0)
    lw x25, 96(a0)
    lw x26, 100(a0)
    lw x27, 104(a0)
    lw x28, 108(a0)
    lw x29, 112(a0)
    lw x30, 116(a0)
    lw x31, 120(a0)
    lw a0, 36(a0)
    addi sp, sp, 128
    mret

.globl do_ecall
do_ecall:
    ecall
    ret
