	.file	"float-stack.c"
	.text
.Ltext0:
	.globl	foo
	.type	foo, @function
foo:
.LFB0:
	.file 1 "float-stack.c"
	.loc 1 6 0
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movss	%xmm0, -4(%rbp)
	movss	%xmm1, -8(%rbp)
	movss	%xmm2, -12(%rbp)
	movss	%xmm3, -16(%rbp)
	movss	%xmm4, -20(%rbp)
	movss	%xmm5, -24(%rbp)
	movss	%xmm6, -28(%rbp)
	movss	%xmm7, -32(%rbp)
	.loc 1 7 0
	movl	$0, %eax
	.loc 1 8 0
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	foo, .-foo
	.globl	_start
	.type	_start, @function
_start:
.LFB1:
	.loc 1 11 0
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$72, %rsp
	.loc 1 14 0
	movl	$17, -8(%rbp)
	.loc 1 16 0
	movl	-8(%rbp), %eax
	movl	%eax, -12(%rbp)
	.loc 1 18 0
	movl	$34, -8(%rbp)
	.loc 1 20 0
	movl	-8(%rbp), %eax
	movl	%eax, -16(%rbp)
	.loc 1 22 0
	movl	$51, -8(%rbp)
	.loc 1 24 0
	movl	-8(%rbp), %eax
	movl	%eax, -20(%rbp)
	.loc 1 26 0
	movl	$68, -8(%rbp)
	.loc 1 28 0
	movl	-8(%rbp), %eax
	movl	%eax, -24(%rbp)
	.loc 1 30 0
	movl	$85, -8(%rbp)
	.loc 1 32 0
	movl	-8(%rbp), %eax
	movl	%eax, -28(%rbp)
	.loc 1 34 0
	movl	$102, -8(%rbp)
	.loc 1 36 0
	movl	-8(%rbp), %eax
	movl	%eax, -32(%rbp)
	.loc 1 38 0
	movl	$119, -8(%rbp)
	.loc 1 40 0
	movl	-8(%rbp), %eax
	movl	%eax, -36(%rbp)
	.loc 1 42 0
	movl	$136, -8(%rbp)
	.loc 1 44 0
	movl	-8(%rbp), %eax
	movl	%eax, -40(%rbp)
	.loc 1 46 0
	movl	$153, -8(%rbp)
	.loc 1 48 0
	movl	-8(%rbp), %eax
	movl	%eax, -44(%rbp)
	.loc 1 50 0
	movl	$170, -8(%rbp)
	.loc 1 52 0
	movl	-8(%rbp), %eax
	movl	%eax, -48(%rbp)
	.loc 1 54 0
	movl	$187, -8(%rbp)
	.loc 1 56 0
	movl	-8(%rbp), %eax
	movl	%eax, -52(%rbp)
	.loc 1 58 0
	movl	$204, -8(%rbp)
	.loc 1 60 0
	movl	-8(%rbp), %eax
	movl	%eax, -56(%rbp)
	.loc 1 62 0
	movss	-56(%rbp), %xmm3
	movss	-52(%rbp), %xmm2
	movss	-48(%rbp), %xmm1
	movss	-44(%rbp), %xmm0
	movss	-40(%rbp), %xmm7
	movss	-36(%rbp), %xmm6
	movss	-32(%rbp), %xmm5
	movss	-28(%rbp), %xmm4
	movss	-24(%rbp), %xmm10
	movss	-20(%rbp), %xmm9
	movss	-16(%rbp), %xmm8
	movl	-12(%rbp), %eax
	leaq	-8(%rsp), %rsp
	movss	%xmm3, (%rsp)
	leaq	-8(%rsp), %rsp
	movss	%xmm2, (%rsp)
	leaq	-8(%rsp), %rsp
	movss	%xmm1, (%rsp)
	leaq	-8(%rsp), %rsp
	movss	%xmm0, (%rsp)
	movaps	%xmm10, %xmm3
	movaps	%xmm9, %xmm2
	movaps	%xmm8, %xmm1
	movl	%eax, -68(%rbp)
	movss	-68(%rbp), %xmm0
	call	foo
	addq	$32, %rsp
	movl	%eax, -4(%rbp)
	.loc 1 63 0
	movl	$0, %eax
	.loc 1 64 0
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	_start, .-_start
.Letext0:
	.file 2 "/usr/include/x86_64-linux-gnu/bits/types.h"
	.file 3 "/usr/include/x86_64-linux-gnu/bits/stdint-intn.h"
	.section	.debug_info,"",@progbits
.Ldebug_info0:
	.long	0x21e
	.value	0x4
	.long	.Ldebug_abbrev0
	.byte	0x8
	.uleb128 0x1
	.long	.LASF12
	.byte	0xc
	.long	.LASF13
	.long	.LASF14
	.quad	.Ltext0
	.quad	.Letext0-.Ltext0
	.long	.Ldebug_line0
	.uleb128 0x2
	.byte	0x1
	.byte	0x8
	.long	.LASF0
	.uleb128 0x2
	.byte	0x2
	.byte	0x7
	.long	.LASF1
	.uleb128 0x2
	.byte	0x4
	.byte	0x7
	.long	.LASF2
	.uleb128 0x2
	.byte	0x8
	.byte	0x7
	.long	.LASF3
	.uleb128 0x2
	.byte	0x1
	.byte	0x6
	.long	.LASF4
	.uleb128 0x2
	.byte	0x2
	.byte	0x5
	.long	.LASF5
	.uleb128 0x3
	.long	.LASF8
	.byte	0x2
	.byte	0x28
	.long	0x62
	.uleb128 0x4
	.byte	0x4
	.byte	0x5
	.string	"int"
	.uleb128 0x2
	.byte	0x8
	.byte	0x5
	.long	.LASF6
	.uleb128 0x2
	.byte	0x1
	.byte	0x6
	.long	.LASF7
	.uleb128 0x3
	.long	.LASF9
	.byte	0x3
	.byte	0x1a
	.long	0x57
	.uleb128 0x5
	.long	.LASF11
	.byte	0x1
	.byte	0xa
	.long	0x62
	.quad	.LFB1
	.quad	.LFE1-.LFB1
	.uleb128 0x1
	.byte	0x9c
	.long	0x15d
	.uleb128 0x6
	.string	"i"
	.byte	0x1
	.byte	0xe
	.long	0x77
	.uleb128 0x2
	.byte	0x91
	.sleb128 -24
	.uleb128 0x6
	.string	"f1"
	.byte	0x1
	.byte	0xf
	.long	0x15d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -28
	.uleb128 0x6
	.string	"f2"
	.byte	0x1
	.byte	0x13
	.long	0x15d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -32
	.uleb128 0x6
	.string	"f3"
	.byte	0x1
	.byte	0x17
	.long	0x15d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -36
	.uleb128 0x6
	.string	"f4"
	.byte	0x1
	.byte	0x1b
	.long	0x15d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -40
	.uleb128 0x6
	.string	"f5"
	.byte	0x1
	.byte	0x1f
	.long	0x15d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -44
	.uleb128 0x6
	.string	"f6"
	.byte	0x1
	.byte	0x23
	.long	0x15d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -48
	.uleb128 0x6
	.string	"f7"
	.byte	0x1
	.byte	0x27
	.long	0x15d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -52
	.uleb128 0x6
	.string	"f8"
	.byte	0x1
	.byte	0x2b
	.long	0x15d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -56
	.uleb128 0x6
	.string	"f9"
	.byte	0x1
	.byte	0x2f
	.long	0x15d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -60
	.uleb128 0x6
	.string	"f10"
	.byte	0x1
	.byte	0x33
	.long	0x15d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -64
	.uleb128 0x6
	.string	"f11"
	.byte	0x1
	.byte	0x37
	.long	0x15d
	.uleb128 0x3
	.byte	0x91
	.sleb128 -68
	.uleb128 0x6
	.string	"f12"
	.byte	0x1
	.byte	0x3b
	.long	0x15d
	.uleb128 0x3
	.byte	0x91
	.sleb128 -72
	.uleb128 0x6
	.string	"x"
	.byte	0x1
	.byte	0x3e
	.long	0x62
	.uleb128 0x2
	.byte	0x91
	.sleb128 -20
	.byte	0
	.uleb128 0x2
	.byte	0x4
	.byte	0x4
	.long	.LASF10
	.uleb128 0x7
	.string	"foo"
	.byte	0x1
	.byte	0x4
	.long	0x62
	.quad	.LFB0
	.quad	.LFE0-.LFB0
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x8
	.string	"p1"
	.byte	0x1
	.byte	0x4
	.long	0x15d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -20
	.uleb128 0x8
	.string	"p2"
	.byte	0x1
	.byte	0x4
	.long	0x15d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -24
	.uleb128 0x8
	.string	"p3"
	.byte	0x1
	.byte	0x4
	.long	0x15d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -28
	.uleb128 0x8
	.string	"p4"
	.byte	0x1
	.byte	0x4
	.long	0x15d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -32
	.uleb128 0x8
	.string	"p5"
	.byte	0x1
	.byte	0x4
	.long	0x15d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -36
	.uleb128 0x8
	.string	"p6"
	.byte	0x1
	.byte	0x4
	.long	0x15d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -40
	.uleb128 0x8
	.string	"p7"
	.byte	0x1
	.byte	0x5
	.long	0x15d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -44
	.uleb128 0x8
	.string	"p8"
	.byte	0x1
	.byte	0x5
	.long	0x15d
	.uleb128 0x2
	.byte	0x91
	.sleb128 -48
	.uleb128 0x8
	.string	"p9"
	.byte	0x1
	.byte	0x5
	.long	0x15d
	.uleb128 0x2
	.byte	0x91
	.sleb128 0
	.uleb128 0x8
	.string	"p10"
	.byte	0x1
	.byte	0x5
	.long	0x15d
	.uleb128 0x2
	.byte	0x91
	.sleb128 8
	.uleb128 0x8
	.string	"p11"
	.byte	0x1
	.byte	0x5
	.long	0x15d
	.uleb128 0x2
	.byte	0x91
	.sleb128 16
	.uleb128 0x8
	.string	"p12"
	.byte	0x1
	.byte	0x5
	.long	0x15d
	.uleb128 0x2
	.byte	0x91
	.sleb128 24
	.byte	0
	.byte	0
	.section	.debug_abbrev,"",@progbits
.Ldebug_abbrev0:
	.uleb128 0x1
	.uleb128 0x11
	.byte	0x1
	.uleb128 0x25
	.uleb128 0xe
	.uleb128 0x13
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x1b
	.uleb128 0xe
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x10
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x2
	.uleb128 0x24
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3e
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0xe
	.byte	0
	.byte	0
	.uleb128 0x3
	.uleb128 0x16
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x4
	.uleb128 0x24
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3e
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0x8
	.byte	0
	.byte	0
	.uleb128 0x5
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x40
	.uleb128 0x18
	.uleb128 0x2116
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x6
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x7
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0x19
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x40
	.uleb128 0x18
	.uleb128 0x2117
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0x8
	.uleb128 0x5
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x18
	.byte	0
	.byte	0
	.byte	0
	.section	.debug_aranges,"",@progbits
	.long	0x2c
	.value	0x2
	.long	.Ldebug_info0
	.byte	0x8
	.byte	0
	.value	0
	.value	0
	.quad	.Ltext0
	.quad	.Letext0-.Ltext0
	.quad	0
	.quad	0
	.section	.debug_line,"",@progbits
.Ldebug_line0:
	.section	.debug_str,"MS",@progbits,1
.LASF10:
	.string	"float"
.LASF5:
	.string	"short int"
.LASF14:
	.string	"/home/karl/work/abide/test/test-data"
.LASF12:
	.string	"GNU C11 7.3.0 -mtune=generic -march=x86-64 -g -O0 -fno-stack-protector"
.LASF6:
	.string	"long int"
.LASF13:
	.string	"float-stack.c"
.LASF0:
	.string	"unsigned char"
.LASF4:
	.string	"signed char"
.LASF2:
	.string	"unsigned int"
.LASF1:
	.string	"short unsigned int"
.LASF7:
	.string	"char"
.LASF9:
	.string	"int32_t"
.LASF11:
	.string	"_start"
.LASF3:
	.string	"long unsigned int"
.LASF8:
	.string	"__int32_t"
	.ident	"GCC: (Ubuntu 7.3.0-27ubuntu1~18.04) 7.3.0"
	.section	.note.GNU-stack,"",@progbits
