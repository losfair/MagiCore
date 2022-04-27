.globl memcpy_u32
memcpy_u32:
	blez	a2,.L1
	slli	a2,a2,2
	addi	a5,a2,-4
	srli	t0,a5,2
	addi	t1,t0,1
	andi	t2,t1,7
	add	a3,a1,a2
	beqz	t2,.L3
	li	a4,1
	beq	t2,a4,.L26
	li	a6,2
	beq	t2,a6,.L27
	li	a7,3
	beq	t2,a7,.L28
	li	t3,4
	beq	t2,t3,.L29
	li	t4,5
	beq	t2,t4,.L30
	li	t5,6
	bne	t2,t5,.L35
.L31:
	lw	a2,0(a1)
	addi	a0,a0,4
	addi	a1,a1,4
	sw	a2,-4(a0)
.L30:
	lw	a5,0(a1)
	addi	a0,a0,4
	addi	a1,a1,4
	sw	a5,-4(a0)
.L29:
	lw	t0,0(a1)
	addi	a0,a0,4
	addi	a1,a1,4
	sw	t0,-4(a0)
.L28:
	lw	t1,0(a1)
	addi	a0,a0,4
	addi	a1,a1,4
	sw	t1,-4(a0)
.L27:
	lw	t2,0(a1)
	addi	a0,a0,4
	addi	a1,a1,4
	sw	t2,-4(a0)
.L26:
	lw	a4,0(a1)
	addi	a1,a1,4
	addi	a0,a0,4
	sw	a4,-4(a0)
	beq	a1,a3,.L36
.L3:
	lw	a6,0(a1)
	addi	a1,a1,32
	addi	a0,a0,32
	lw	a7,-28(a1)
	lw	t3,-24(a1)
	lw	t4,-20(a1)
	lw	t5,-16(a1)
	lw	t6,-12(a1)
	lw	a2,-8(a1)
	lw	a5,-4(a1)
	sw	a6,-32(a0)
	sw	a7,-28(a0)
	sw	t3,-24(a0)
	sw	t4,-20(a0)
	sw	t5,-16(a0)
	sw	t6,-12(a0)
	sw	a2,-8(a0)
	sw	a5,-4(a0)
	bne	a1,a3,.L3
.L1:
	ret
.L35:
	lw	t6,0(a1)
	addi	a0,a0,4
	addi	a1,a1,4
	sw	t6,-4(a0)
	j	.L31
.L36:
	ret