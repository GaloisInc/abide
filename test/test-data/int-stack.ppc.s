	.file	"int-stack.c"
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
	std 3,112(31)
	std 4,120(31)
	std 5,128(31)
	std 6,136(31)
	std 7,144(31)
	std 8,152(31)
	std 9,160(31)
	std 10,168(31)
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
	li 9,170
	std 9,120(1)
	li 9,153
	std 9,112(1)
	li 10,136
	li 9,119
	li 8,102
	li 7,85
	li 6,68
	li 5,51
	li 4,34
	li 3,17
	bl foo
	mr 9,3
	stw 9,140(31)
	li 9,0
	mr 3,9
	addi 1,31,160
	ld 0,16(1)
	mtlr 0
	ld 31,-8(1)
	blr
	.long 0
	.byte 0,0,0,1,128,1,0,1
	.size	main,.-.L.main
	.ident	"GCC: (Ubuntu 7.3.0-27ubuntu1~18.04) 7.3.0"
