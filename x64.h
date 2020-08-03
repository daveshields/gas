# copyright 1987-2012 robert b. k. dewar and mark emmer.

# copyright 2012-2020 david shields
#
# this file is part of macro spitbol.
#
#     macro spitbol is free software: you can redistribute it and/or modify
#     it under the terms of the gnu general public license as published by
#     the free software foundation, either version 2 of the license, or
#     (at your option) any later version.
#
#     macro spitbol is distributed in the hope that it will be useful,
#     but without any warranty; without even the implied warranty of
#     merchantability or fitness for a particular purpose.  see the
#     gnu general public license for more details.
#
#     you should have received a copy of the gnu general public license
#     along with macro spitbol.  if not, see <http://www.gnu.org/licenses/>.

	%define m_char	byte	; reference to byte in memory
	%define d_char	db	; define value of byte
	%define m_real	qword	; reference to floating point value in memory
	%define d_real	dq	; define value for floating point

	.equ	xl	rsi
	.equ	xt	rsi
	.equ	xr	rdi
	.equ	w0	rax
	%define w1	rbp
	.equ	wa	rcx
	%define wa_l	cl
	.equ	wb	rbx
	%define wb_l    bl
	.equ	wc	rdx
	%define wc_l    dl
	.equ	xs	rsp
	.equ	w0_l	al
#	.equ	ia	rdx
	.equ	ia	rbp
	%define m_word  qword
	%define d_word	dq
#	.equ	cfp_b	8
	%define log_cfp_b 3
	%define log_cfp_c 3
	%define d_real	dq
	%define cfp_c_val	8
	%define log_cfp_c 3
	%define cfp_m_	9223372036854775807
#	.equ	cfp_n_	64

	.equ	lods_w	lodsq
	.equ	lods_b	lodsb
	%define movs_b	movsb
	%define movs_w	movsq
	%define stos_b	stosb
	.equ	stos_w	stosq
	.equ	cmps_b	cmpsb

	.equ	cdq	cqo	; sign extend (64 bits)

#	flags
	.equ	flag_of	0x80
	.equ	flag_cf	0x01
	.equ	flag_ca	0x40
