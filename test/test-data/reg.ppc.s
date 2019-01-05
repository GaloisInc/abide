	.file	"reg.c"
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
	mr 9,3
	mr 10,4
	std 5,128(31)
	stfs 1,136(31)
	stw 9,112(31)
	mr 9,10
	stb 9,120(31)
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
	stdu 1,-160(1)
	mr 31,1
	ld 9,-28688(13)
	std 9,136(31)
	li 9,0
	li 9,68
	stw 9,124(31)
	lwz 9,124(31)
	stw 9,128(31)
	lfs 0,128(31)
	fmr 1,0
	li 5,51
	li 4,34
	li 3,17
	bl foo
	mr 9,3
	stw 9,132(31)
	li 9,0
	mr 3,9
	ld 9,136(31)
	ld 10,-28688(13)
	cmpld 7,9,10
	li 9,0
	li 10,0
	beq 7,.L5
	bl __stack_chk_fail
	nop
.L5:
	addi 1,31,160
	ld 0,16(1)
	mtlr 0
	ld 31,-8(1)
	blr
	.long 0
	.byte 0,0,0,1,128,1,0,1
	.size	main,.-.L.main
	.ident	"GCC: (Ubuntu 7.3.0-27ubuntu1~18.04) 7.3.0"
	.gnu_attribute 4, 1
