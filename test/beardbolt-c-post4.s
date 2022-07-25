isRMS:
	sub	edi, 77
	mov	eax, 200
	cmp	edi, 6
	ja	.L1
	lea	rax, CSWTCH.1[rip]
	mov	eax, DWORD PTR [rax+rdi*4]
.L1:
	rep ret
.LC0:
	.string	"%c\n"
main:
	lea	rdi, .LC0[rip]
	sub	rsp, 8
	mov	esi, 2
	xor	eax, eax
	call	printf@PLT
	xor	eax, eax
	add	rsp, 8
	ret
CSWTCH.1:
	.long	2
	.long	200
	.long	200
	.long	200
	.long	200
	.long	1
	.long	3
