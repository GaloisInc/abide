	.file	"float-stack.c"
	.section	".text"
	.align 2
	.globl foo
	.section	".opd","aw"
	.align 3
foo:
	.quad	.L.foo,.TOC.@tocbase,0
	.previous
	.type	foo, @function
.L.foo:
	std 31,-8(1)
	stdu 1,-64(1)
	mr 31,1
	stfs 1,112(31)
	stfs 2,120(31)
	stfs 3,128(31)
	stfs 4,136(31)
	stfs 5,144(31)
	stfs 6,152(31)
	stfs 7,160(31)
	stfs 8,168(31)
	stfs 9,176(31)
	stfs 10,184(31)
	stfs 11,192(31)
	stfs 12,200(31)
	li 9,0
	mr 3,9
	addi 1,31,64
	ld 31,-8(1)
	blr
	.long 0
	.byte 0,0,0,0,128,1,0,1
	.size	foo,.-.L.foo
	.align 2
	.globl main
	.section	".opd","aw"
	.align 3
main:
	.quad	.L.main,.TOC.@tocbase,0
	.previous
	.type	main, @function
.L.main:
	mflr 0
	std 0,16(1)
	std 31,-8(1)
	stdu 1,-224(1)
	mr 31,1
	ld 9,-28688(13)
	std 9,200(31)
	li 9,0
	li 9,17
	stw 9,144(31)
	lwz 9,144(31)
	stw 9,148(31)
	li 9,34
	stw 9,144(31)
	lwz 9,144(31)
	stw 9,152(31)
	li 9,51
	stw 9,144(31)
	lwz 9,144(31)
	stw 9,156(31)
	li 9,68
	stw 9,144(31)
	lwz 9,144(31)
	stw 9,160(31)
	li 9,85
	stw 9,144(31)
	lwz 9,144(31)
	stw 9,164(31)
	li 9,102
	stw 9,144(31)
	lwz 9,144(31)
	stw 9,168(31)
	li 9,119
	stw 9,144(31)
	lwz 9,144(31)
	stw 9,172(31)
	li 9,136
	stw 9,144(31)
	lwz 9,144(31)
	stw 9,176(31)
	li 9,153
	stw 9,144(31)
	lwz 9,144(31)
	stw 9,180(31)
	li 9,170
	stw 9,144(31)
	lwz 9,144(31)
	stw 9,184(31)
	li 9,187
	stw 9,144(31)
	lwz 9,144(31)
	stw 9,188(31)
	li 9,204
	stw 9,144(31)
	lwz 9,144(31)
	stw 9,192(31)
	lfs 0,148(31)
	lfs 2,152(31)
	lfs 3,156(31)
	lfs 4,160(31)
	lfs 5,164(31)
	lfs 6,168(31)
	lfs 7,172(31)
	lfs 8,176(31)
	lfs 9,180(31)
	lfs 10,184(31)
	lfs 11,188(31)
	lfs 12,192(31)
	fmr 1,0
	bl foo
	mr 9,3
	stw 9,196(31)
	li 9,0
	mr 3,9
	ld 9,200(31)
	ld 10,-28688(13)
	cmpld 7,9,10
	li 9,0
	li 10,0
	beq 7,.L5
	bl __stack_chk_fail
	nop
.L5:
	addi 1,31,224
	ld 0,16(1)
	mtlr 0
	ld 31,-8(1)
	blr
	.long 0
	.byte 0,0,0,1,128,1,0,1
	.size	main,.-.L.main
	.ident	"GCC: (Ubuntu 7.3.0-27ubuntu1~18.04) 7.3.0"
	.gnu_attribute 4, 1
