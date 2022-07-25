isRMS:
	push	rbp
	mov	rbp, rsp
	mov	DWORD PTR -4[rbp], edi
	mov	eax, DWORD PTR -4[rbp]
	cmp	eax, 82
	je	.L3
	cmp	eax, 83
	je	.L4
	cmp	eax, 77
	je	.L5
	jmp	.L7
.L3:
	mov	eax, 1
	jmp	.L6
.L5:
	mov	eax, 2
	jmp	.L6
.L4:
	mov	eax, 3
	jmp	.L6
.L7:
	mov	eax, 200
.L6:
	pop	rbp
	ret
.LC0:
	.string	"%c\n"
main:
	push	rbp
	mov	rbp, rsp
	sub	rsp, 16
	mov	BYTE PTR -1[rbp], 2
	movsx	eax, BYTE PTR -1[rbp]
	mov	edi, eax
	call	isRMS
	test	eax, eax
	je	.L9
	movsx	eax, BYTE PTR -1[rbp]
	mov	esi, eax
	lea	rdi, .LC0[rip]
	mov	eax, 0
	call	printf@PLT
.L9:
	mov	eax, 0
	leave
	ret
