	.file	"float-stack.c"
	.text
	.globl	foo
	.type	foo, @function
foo:
.LFB0:
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
	movl	$0, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	foo, .-foo
	.globl	main
	.type	main, @function
main:
.LFB1:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$80, %rsp
	movq	%fs:40, %rax
	movq	%rax, -8(%rbp)
	xorl	%eax, %eax
	movl	$17, -64(%rbp)
	movl	-64(%rbp), %eax
	movl	%eax, -60(%rbp)
	movl	$34, -64(%rbp)
	movl	-64(%rbp), %eax
	movl	%eax, -56(%rbp)
	movl	$51, -64(%rbp)
	movl	-64(%rbp), %eax
	movl	%eax, -52(%rbp)
	movl	$68, -64(%rbp)
	movl	-64(%rbp), %eax
	movl	%eax, -48(%rbp)
	movl	$85, -64(%rbp)
	movl	-64(%rbp), %eax
	movl	%eax, -44(%rbp)
	movl	$102, -64(%rbp)
	movl	-64(%rbp), %eax
	movl	%eax, -40(%rbp)
	movl	$119, -64(%rbp)
	movl	-64(%rbp), %eax
	movl	%eax, -36(%rbp)
	movl	$136, -64(%rbp)
	movl	-64(%rbp), %eax
	movl	%eax, -32(%rbp)
	movl	$153, -64(%rbp)
	movl	-64(%rbp), %eax
	movl	%eax, -28(%rbp)
	movl	$170, -64(%rbp)
	movl	-64(%rbp), %eax
	movl	%eax, -24(%rbp)
	movl	$187, -64(%rbp)
	movl	-64(%rbp), %eax
	movl	%eax, -20(%rbp)
	movl	$204, -64(%rbp)
	movl	-64(%rbp), %eax
	movl	%eax, -16(%rbp)
	movss	-16(%rbp), %xmm3
	movss	-20(%rbp), %xmm2
	movss	-24(%rbp), %xmm1
	movss	-28(%rbp), %xmm0
	movss	-32(%rbp), %xmm7
	movss	-36(%rbp), %xmm6
	movss	-40(%rbp), %xmm5
	movss	-44(%rbp), %xmm4
	movss	-48(%rbp), %xmm10
	movss	-52(%rbp), %xmm9
	movss	-56(%rbp), %xmm8
	movl	-60(%rbp), %eax
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
	movl	%eax, -12(%rbp)
	movl	$0, %eax
	movq	-8(%rbp), %rdx
	xorq	%fs:40, %rdx
	je	.L5
	call	__stack_chk_fail@PLT
.L5:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 7.3.0-27ubuntu1~18.04) 7.3.0"
	.section	.note.GNU-stack,"",@progbits
