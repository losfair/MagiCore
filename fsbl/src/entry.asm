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
    addi sp, sp, -248
    sd x1, 0(sp)
    sd x2, 8(sp)
    sd x3, 16(sp)
    sd x4, 24(sp)
    sd x5, 32(sp)
    sd x6, 40(sp)
    sd x7, 48(sp)
    sd x8, 56(sp)
    sd x9, 64(sp)
    sd x10, 72(sp)
    sd x11, 80(sp)
    sd x12, 88(sp)
    sd x13, 96(sp)
    sd x14, 104(sp)
    sd x15, 112(sp)
    sd x16, 120(sp)
    sd x17, 128(sp)
    sd x18, 136(sp)
    sd x19, 144(sp)
    sd x20, 152(sp)
    sd x21, 160(sp)
    sd x22, 168(sp)
    sd x23, 176(sp)
    sd x24, 184(sp)
    sd x25, 192(sp)
    sd x26, 200(sp)
    sd x27, 208(sp)
    sd x28, 216(sp)
    sd x29, 224(sp)
    sd x30, 232(sp)
    sd x31, 240(sp)

    mv a0, sp

    call rust_intr_entry

    mv a0, sp

.globl asm_return_from_interrupt
asm_return_from_interrupt:
    ld x1, 0(a0)
    ld x2, 8(a0)
    ld x3, 16(a0)
    ld x4, 24(a0)
    ld x5, 32(a0)
    ld x6, 40(a0)
    ld x7, 48(a0)
    ld x8, 56(a0)
    ld x9, 64(a0)
    # ld x10, 72(a0)
    ld x11, 80(a0)
    ld x12, 88(a0)
    ld x13, 96(a0)
    ld x14, 104(a0)
    ld x15, 112(a0)
    ld x16, 120(a0)
    ld x17, 128(a0)
    ld x18, 136(a0)
    ld x19, 144(a0)
    ld x20, 152(a0)
    ld x21, 160(a0)
    ld x22, 168(a0)
    ld x23, 176(a0)
    ld x24, 184(a0)
    ld x25, 192(a0)
    ld x26, 200(a0)
    ld x27, 208(a0)
    ld x28, 216(a0)
    ld x29, 224(a0)
    ld x30, 232(a0)
    ld x31, 240(a0)
    ld a0, 72(a0)
    addi sp, sp, 248
    mret

.globl do_ecall
do_ecall:
    ecall
    ret

.globl test_amo_add_w
test_amo_add_w:
    li t0, 1
    amoadd.w a0, t0, (a0)
    ret

.globl test_amo_swap_w
test_amo_swap_w:
    li t0, 35
    amoswap.w a0, t0, (a0)
    ret

.globl test_amo_max_w
test_amo_max_w:
    li t0, 22
    amomax.w a0, t0, (a0)
    ret
