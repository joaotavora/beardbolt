.Ltext0:
isRMS:
.LFB0:
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
.LFE0:
.LC0:
	.string	"%c\n"
main:
.LFB1:
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
.LFE1:
.Letext0:
.Ldebug_info0:
.Ldebug_abbrev0:
.Ldebug_line0:
.LASF20:
.LASF28:
.LASF55:
.LASF52:
.LASF23:
.LASF5:
.LASF7:
.LASF10:
.LASF33:
.LASF17:
.LASF12:
.LASF19:
.LASF24:
.LASF14:
.LASF57:
.LASF51:
.LASF58:
.LASF32:
.LASF6:
.LASF29:
.LASF48:
.LASF60:
.LASF45:
.LASF44:
.LASF41:
.LASF1:
.LASF4:
.LASF46:
.LASF3:
.LASF42:
.LASF31:
.LASF16:
.LASF40:
.LASF13:
.LASF2:
.LASF11:
.LASF54:
.LASF43:
.LASF34:
.LASF35:
.LASF36:
.LASF37:
.LASF38:
.LASF0:
.LASF18:
.LASF9:
.LASF8:
.LASF25:
.LASF22:
.LASF49:
.LASF27:
.LASF39:
.LASF15:
.LASF30:
.LASF21:
.LASF53:
.LASF26:
.LASF56:
.LASF50:
.LASF47:
.LASF59:
