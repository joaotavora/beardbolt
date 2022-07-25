	.file	"beardbolt.c"
	.intel_syntax noprefix
	.text
.Ltext0:
	.p2align 4,,15
	.globl	isRMS
	.type	isRMS, @function
isRMS:
.LFB11:
	.file 1 "/tmp/beardbolt-LsubiK/beardbolt.c"
	.loc 1 11 0
	.cfi_startproc
.LVL0:
	sub	edi, 77
.LVL1:
	.loc 1 12 0
	mov	eax, 200
	cmp	edi, 6
	ja	.L1
	lea	rax, CSWTCH.1[rip]
	mov	eax, DWORD PTR [rax+rdi*4]
.L1:
	.loc 1 22 0
	rep ret
	.cfi_endproc
.LFE11:
	.size	isRMS, .-isRMS
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"%c\n"
	.section	.text.startup,"ax",@progbits
	.p2align 4,,15
	.globl	main
	.type	main, @function
main:
.LFB12:
	.loc 1 24 0
	.cfi_startproc
.LVL2:
	.loc 1 27 0
	lea	rdi, .LC0[rip]
	.loc 1 24 0
	sub	rsp, 8
	.cfi_def_cfa_offset 16
	.loc 1 27 0
	mov	esi, 2
	xor	eax, eax
	call	printf@PLT
.LVL3:
	.loc 1 28 0
	xor	eax, eax
	add	rsp, 8
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE12:
	.size	main, .-main
	.section	.rodata
	.align 16
	.type	CSWTCH.1, @object
	.size	CSWTCH.1, 28
CSWTCH.1:
	.long	2
	.long	200
	.long	200
	.long	200
	.long	200
	.long	1
	.long	3
	.text
.Letext0:
	.file 2 "/usr/lib/gcc/x86_64-linux-gnu/6/include/stddef.h"
	.file 3 "/usr/include/x86_64-linux-gnu/bits/types.h"
	.file 4 "/usr/include/libio.h"
	.file 5 "/usr/include/stdio.h"
	.file 6 "/usr/include/x86_64-linux-gnu/bits/sys_errlist.h"
	.section	.debug_info,"",@progbits
.Ldebug_info0:
	.long	0x393
	.value	0x4
	.long	.Ldebug_abbrev0
	.byte	0x8
	.uleb128 0x1
	.long	.LASF54
	.byte	0xc
	.long	.LASF55
	.long	.LASF56
	.long	.Ldebug_ranges0+0
	.quad	0
	.long	.Ldebug_line0
	.uleb128 0x2
	.long	.LASF7
	.byte	0x2
	.byte	0xd8
	.long	0x34
	.uleb128 0x3
	.byte	0x8
	.byte	0x7
	.long	.LASF0
	.uleb128 0x3
	.byte	0x1
	.byte	0x8
	.long	.LASF1
	.uleb128 0x3
	.byte	0x2
	.byte	0x7
	.long	.LASF2
	.uleb128 0x3
	.byte	0x4
	.byte	0x7
	.long	.LASF3
	.uleb128 0x3
	.byte	0x1
	.byte	0x6
	.long	.LASF4
	.uleb128 0x3
	.byte	0x2
	.byte	0x5
	.long	.LASF5
	.uleb128 0x4
	.byte	0x4
	.byte	0x5
	.string	"int"
	.uleb128 0x3
	.byte	0x8
	.byte	0x5
	.long	.LASF6
	.uleb128 0x2
	.long	.LASF8
	.byte	0x3
	.byte	0x83
	.long	0x65
	.uleb128 0x2
	.long	.LASF9
	.byte	0x3
	.byte	0x84
	.long	0x65
	.uleb128 0x3
	.byte	0x8
	.byte	0x7
	.long	.LASF10
	.uleb128 0x5
	.byte	0x8
	.uleb128 0x6
	.byte	0x8
	.long	0x91
	.uleb128 0x3
	.byte	0x1
	.byte	0x6
	.long	.LASF11
	.uleb128 0x7
	.long	0x91
	.uleb128 0x8
	.long	.LASF41
	.byte	0xd8
	.byte	0x4
	.byte	0xf1
	.long	0x21a
	.uleb128 0x9
	.long	.LASF12
	.byte	0x4
	.byte	0xf2
	.long	0x5e
	.byte	0
	.uleb128 0x9
	.long	.LASF13
	.byte	0x4
	.byte	0xf7
	.long	0x8b
	.byte	0x8
	.uleb128 0x9
	.long	.LASF14
	.byte	0x4
	.byte	0xf8
	.long	0x8b
	.byte	0x10
	.uleb128 0x9
	.long	.LASF15
	.byte	0x4
	.byte	0xf9
	.long	0x8b
	.byte	0x18
	.uleb128 0x9
	.long	.LASF16
	.byte	0x4
	.byte	0xfa
	.long	0x8b
	.byte	0x20
	.uleb128 0x9
	.long	.LASF17
	.byte	0x4
	.byte	0xfb
	.long	0x8b
	.byte	0x28
	.uleb128 0x9
	.long	.LASF18
	.byte	0x4
	.byte	0xfc
	.long	0x8b
	.byte	0x30
	.uleb128 0x9
	.long	.LASF19
	.byte	0x4
	.byte	0xfd
	.long	0x8b
	.byte	0x38
	.uleb128 0x9
	.long	.LASF20
	.byte	0x4
	.byte	0xfe
	.long	0x8b
	.byte	0x40
	.uleb128 0xa
	.long	.LASF21
	.byte	0x4
	.value	0x100
	.long	0x8b
	.byte	0x48
	.uleb128 0xa
	.long	.LASF22
	.byte	0x4
	.value	0x101
	.long	0x8b
	.byte	0x50
	.uleb128 0xa
	.long	.LASF23
	.byte	0x4
	.value	0x102
	.long	0x8b
	.byte	0x58
	.uleb128 0xa
	.long	.LASF24
	.byte	0x4
	.value	0x104
	.long	0x252
	.byte	0x60
	.uleb128 0xa
	.long	.LASF25
	.byte	0x4
	.value	0x106
	.long	0x258
	.byte	0x68
	.uleb128 0xa
	.long	.LASF26
	.byte	0x4
	.value	0x108
	.long	0x5e
	.byte	0x70
	.uleb128 0xa
	.long	.LASF27
	.byte	0x4
	.value	0x10c
	.long	0x5e
	.byte	0x74
	.uleb128 0xa
	.long	.LASF28
	.byte	0x4
	.value	0x10e
	.long	0x6c
	.byte	0x78
	.uleb128 0xa
	.long	.LASF29
	.byte	0x4
	.value	0x112
	.long	0x42
	.byte	0x80
	.uleb128 0xa
	.long	.LASF30
	.byte	0x4
	.value	0x113
	.long	0x50
	.byte	0x82
	.uleb128 0xa
	.long	.LASF31
	.byte	0x4
	.value	0x114
	.long	0x25e
	.byte	0x83
	.uleb128 0xa
	.long	.LASF32
	.byte	0x4
	.value	0x118
	.long	0x26e
	.byte	0x88
	.uleb128 0xa
	.long	.LASF33
	.byte	0x4
	.value	0x121
	.long	0x77
	.byte	0x90
	.uleb128 0xa
	.long	.LASF34
	.byte	0x4
	.value	0x129
	.long	0x89
	.byte	0x98
	.uleb128 0xa
	.long	.LASF35
	.byte	0x4
	.value	0x12a
	.long	0x89
	.byte	0xa0
	.uleb128 0xa
	.long	.LASF36
	.byte	0x4
	.value	0x12b
	.long	0x89
	.byte	0xa8
	.uleb128 0xa
	.long	.LASF37
	.byte	0x4
	.value	0x12c
	.long	0x89
	.byte	0xb0
	.uleb128 0xa
	.long	.LASF38
	.byte	0x4
	.value	0x12e
	.long	0x29
	.byte	0xb8
	.uleb128 0xa
	.long	.LASF39
	.byte	0x4
	.value	0x12f
	.long	0x5e
	.byte	0xc0
	.uleb128 0xa
	.long	.LASF40
	.byte	0x4
	.value	0x131
	.long	0x274
	.byte	0xc4
	.byte	0
	.uleb128 0xb
	.long	.LASF57
	.byte	0x4
	.byte	0x96
	.uleb128 0x8
	.long	.LASF42
	.byte	0x18
	.byte	0x4
	.byte	0x9c
	.long	0x252
	.uleb128 0x9
	.long	.LASF43
	.byte	0x4
	.byte	0x9d
	.long	0x252
	.byte	0
	.uleb128 0x9
	.long	.LASF44
	.byte	0x4
	.byte	0x9e
	.long	0x258
	.byte	0x8
	.uleb128 0x9
	.long	.LASF45
	.byte	0x4
	.byte	0xa2
	.long	0x5e
	.byte	0x10
	.byte	0
	.uleb128 0x6
	.byte	0x8
	.long	0x221
	.uleb128 0x6
	.byte	0x8
	.long	0x9d
	.uleb128 0xc
	.long	0x91
	.long	0x26e
	.uleb128 0xd
	.long	0x82
	.byte	0
	.byte	0
	.uleb128 0x6
	.byte	0x8
	.long	0x21a
	.uleb128 0xc
	.long	0x91
	.long	0x284
	.uleb128 0xd
	.long	0x82
	.byte	0x13
	.byte	0
	.uleb128 0xe
	.long	.LASF58
	.uleb128 0xf
	.long	.LASF46
	.byte	0x4
	.value	0x13b
	.long	0x284
	.uleb128 0xf
	.long	.LASF47
	.byte	0x4
	.value	0x13c
	.long	0x284
	.uleb128 0xf
	.long	.LASF48
	.byte	0x4
	.value	0x13d
	.long	0x284
	.uleb128 0x6
	.byte	0x8
	.long	0x98
	.uleb128 0x7
	.long	0x2ad
	.uleb128 0x10
	.long	.LASF49
	.byte	0x5
	.byte	0xaa
	.long	0x258
	.uleb128 0x10
	.long	.LASF50
	.byte	0x5
	.byte	0xab
	.long	0x258
	.uleb128 0x10
	.long	.LASF51
	.byte	0x5
	.byte	0xac
	.long	0x258
	.uleb128 0x10
	.long	.LASF52
	.byte	0x6
	.byte	0x1a
	.long	0x5e
	.uleb128 0xc
	.long	0x2b3
	.long	0x2ef
	.uleb128 0x11
	.byte	0
	.uleb128 0x7
	.long	0x2e4
	.uleb128 0x10
	.long	.LASF53
	.byte	0x6
	.byte	0x1b
	.long	0x2ef
	.uleb128 0x12
	.long	.LASF59
	.byte	0x1
	.byte	0x18
	.long	0x5e
	.quad	.LFB12
	.quad	.LFE12-.LFB12
	.uleb128 0x1
	.byte	0x9c
	.long	0x34b
	.uleb128 0x13
	.string	"a"
	.byte	0x1
	.byte	0x19
	.long	0x91
	.byte	0x2
	.uleb128 0x14
	.quad	.LVL3
	.long	0x38a
	.uleb128 0x15
	.uleb128 0x1
	.byte	0x55
	.uleb128 0x9
	.byte	0x3
	.quad	.LC0
	.uleb128 0x15
	.uleb128 0x1
	.byte	0x54
	.uleb128 0x1
	.byte	0x32
	.byte	0
	.byte	0
	.uleb128 0x16
	.long	.LASF60
	.byte	0x1
	.byte	0xb
	.long	0x5e
	.byte	0x1
	.long	0x365
	.uleb128 0x17
	.string	"a"
	.byte	0x1
	.byte	0xb
	.long	0x5e
	.byte	0
	.uleb128 0x18
	.long	0x34b
	.quad	.LFB11
	.quad	.LFE11-.LFB11
	.uleb128 0x1
	.byte	0x9c
	.long	0x38a
	.uleb128 0x19
	.long	0x35b
	.long	.LLST0
	.byte	0
	.uleb128 0x1a
	.long	.LASF61
	.long	.LASF61
	.byte	0x5
	.value	0x16c
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
	.uleb128 0x55
	.uleb128 0x17
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x10
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x2
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
	.uleb128 0x3
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
	.uleb128 0xf
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x6
	.uleb128 0xf
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x7
	.uleb128 0x26
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x8
	.uleb128 0x13
	.byte	0x1
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x9
	.uleb128 0xd
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x38
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0xa
	.uleb128 0xd
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x38
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0xb
	.uleb128 0x16
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0xc
	.uleb128 0x1
	.byte	0x1
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0xd
	.uleb128 0x21
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2f
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0xe
	.uleb128 0x13
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3c
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0xf
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3c
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0x10
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3c
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0x11
	.uleb128 0x21
	.byte	0
	.byte	0
	.byte	0
	.uleb128 0x12
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
	.uleb128 0x2117
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x13
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
	.uleb128 0x1c
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x14
	.uleb128 0x4109
	.byte	0x1
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x31
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x15
	.uleb128 0x410a
	.byte	0
	.uleb128 0x2
	.uleb128 0x18
	.uleb128 0x2111
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x16
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
	.uleb128 0x27
	.uleb128 0x19
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x20
	.uleb128 0xb
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x17
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
	.byte	0
	.byte	0
	.uleb128 0x18
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x31
	.uleb128 0x13
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x40
	.uleb128 0x18
	.uleb128 0x2117
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x19
	.uleb128 0x5
	.byte	0
	.uleb128 0x31
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x1a
	.uleb128 0x2e
	.byte	0
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3c
	.uleb128 0x19
	.uleb128 0x6e
	.uleb128 0xe
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0x5
	.byte	0
	.byte	0
	.byte	0
	.section	.debug_loc,"",@progbits
.Ldebug_loc0:
.LLST0:
	.quad	.LVL0
	.quad	.LVL1
	.value	0x1
	.byte	0x55
	.quad	.LVL1
	.quad	.LFE11
	.value	0x4
	.byte	0xf3
	.uleb128 0x1
	.byte	0x55
	.byte	0x9f
	.quad	0
	.quad	0
	.section	.debug_aranges,"",@progbits
	.long	0x3c
	.value	0x2
	.long	.Ldebug_info0
	.byte	0x8
	.byte	0
	.value	0
	.value	0
	.quad	.Ltext0
	.quad	.Letext0-.Ltext0
	.quad	.LFB12
	.quad	.LFE12-.LFB12
	.quad	0
	.quad	0
	.section	.debug_ranges,"",@progbits
.Ldebug_ranges0:
	.quad	.Ltext0
	.quad	.Letext0
	.quad	.LFB12
	.quad	.LFE12
	.quad	0
	.quad	0
	.section	.debug_line,"",@progbits
.Ldebug_line0:
	.section	.debug_str,"MS",@progbits,1
.LASF20:
	.string	"_IO_buf_end"
.LASF28:
	.string	"_old_offset"
.LASF60:
	.string	"isRMS"
.LASF52:
	.string	"sys_nerr"
.LASF23:
	.string	"_IO_save_end"
.LASF5:
	.string	"short int"
.LASF7:
	.string	"size_t"
.LASF10:
	.string	"sizetype"
.LASF33:
	.string	"_offset"
.LASF17:
	.string	"_IO_write_ptr"
.LASF12:
	.string	"_flags"
.LASF19:
	.string	"_IO_buf_base"
.LASF24:
	.string	"_markers"
.LASF14:
	.string	"_IO_read_end"
.LASF55:
	.string	"/tmp/beardbolt-LsubiK/beardbolt.c"
.LASF51:
	.string	"stderr"
.LASF56:
	.string	"/tmp/beardbolt-LsubiK"
.LASF32:
	.string	"_lock"
.LASF6:
	.string	"long int"
.LASF61:
	.string	"printf"
.LASF29:
	.string	"_cur_column"
.LASF48:
	.string	"_IO_2_1_stderr_"
.LASF58:
	.string	"_IO_FILE_plus"
.LASF45:
	.string	"_pos"
.LASF44:
	.string	"_sbuf"
.LASF41:
	.string	"_IO_FILE"
.LASF1:
	.string	"unsigned char"
.LASF4:
	.string	"signed char"
.LASF46:
	.string	"_IO_2_1_stdin_"
.LASF3:
	.string	"unsigned int"
.LASF42:
	.string	"_IO_marker"
.LASF31:
	.string	"_shortbuf"
.LASF16:
	.string	"_IO_write_base"
.LASF40:
	.string	"_unused2"
.LASF13:
	.string	"_IO_read_ptr"
.LASF2:
	.string	"short unsigned int"
.LASF11:
	.string	"char"
.LASF59:
	.string	"main"
.LASF43:
	.string	"_next"
.LASF34:
	.string	"__pad1"
.LASF35:
	.string	"__pad2"
.LASF36:
	.string	"__pad3"
.LASF37:
	.string	"__pad4"
.LASF38:
	.string	"__pad5"
.LASF0:
	.string	"long unsigned int"
.LASF18:
	.string	"_IO_write_end"
.LASF9:
	.string	"__off64_t"
.LASF8:
	.string	"__off_t"
.LASF25:
	.string	"_chain"
.LASF22:
	.string	"_IO_backup_base"
.LASF49:
	.string	"stdin"
.LASF27:
	.string	"_flags2"
.LASF39:
	.string	"_mode"
.LASF15:
	.string	"_IO_read_base"
.LASF30:
	.string	"_vtable_offset"
.LASF21:
	.string	"_IO_save_base"
.LASF53:
	.string	"sys_errlist"
.LASF26:
	.string	"_fileno"
.LASF54:
	.string	"GNU C11 6.3.0 20170516 -masm=intel -mtune=generic -march=x86-64 -g -O3"
.LASF50:
	.string	"stdout"
.LASF47:
	.string	"_IO_2_1_stdout_"
.LASF57:
	.string	"_IO_lock_t"
	.ident	"GCC: (Debian 6.3.0-18+deb9u1) 6.3.0 20170516"
	.section	.note.GNU-stack,"",@progbits
