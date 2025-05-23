## -*- asm -*- ##############################################################
#                                                                           #
#                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  #
#                                                                           #
#                                 S T A R T                                 #
#                                                                           #
#                               Assembly File                               #
#                                                                           #
#                      Copyright (C) 2012-2016 AdaCore                      #
#                                                                           #
#  GNAT is free software;  you can  redistribute it  and/or modify it under #
#  terms of the  GNU General Public License as published  by the Free Soft- #
#  ware  Foundation;  either version 3,  or (at your option) any later ver- #
#  sion.  GNAT is distributed in the hope that it will be useful, but WITH- #
#  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY #
#  or FITNESS FOR A PARTICULAR PURPOSE.                                     #
#                                                                           #
#  As a special exception under Section 7 of GPL version 3, you are granted #
#  additional permissions described in the GCC Runtime Library Exception,   #
#  version 3.1, as published by the Free Software Foundation.               #
#                                                                           #
#  You should have received a copy of the GNU General Public License and    #
#  a copy of the GCC Runtime Library Exception along with this program;     #
#  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    #
#  <http://www.gnu.org/licenses/>.                                          #
#                                                                           #
#############################################################################

	.syntax unified
	.cpu cortex-m4
	.thumb

	.section .vectors,"a"
__vectors0:
	.word   __stack_end /* stack top address */
	.word   _start_ram  /* 1 Reset */
	.word   hang        /* 2 NMI */
	.word   hang        /* 3 HardFault */
	.word   hang        /* 4 MemManage */

	.text

	.thumb_func
	.globl _start_ram
_start_ram:
	movw	r0, #:lower16:__kalico_boot_flag
	movt	r0, #:upper16:__kalico_boot_flag
	ldr	r1, [r0]
	movw	r2, #0x93F0
	movt	r2, #0x0B1C
	cmp	r1, r2
	bne	prunt_boot
	mov	r1, #0
	str	r1, [r0]
	movw	r0, #0x2000
	movt	r0, #0x0803
	ldr	sp, [r0]
	movw	r0, #0x2004
	movt	r0, #0x0803
	ldr	r0, [r0]
	bx	r0

prunt_boot:
	/* Init stack */
	ldr	sp,.LinitSp

	/* Enable FPU and cache when appropriate */
	bl	_stm32_start_common

	/* Clear .bss */
	movw	r0,#:lower16:__bss_start
	movt	r0,#:upper16:__bss_start
	movw	r1,#:lower16:__bss_words
	mov	r2,#0
	cbz	r1,1f
0:	str	r2,[r0],#4
	subs	r1,r1,#1
	bne	0b
1:
	/* Clear .ccmbss */
	movw	r0,#:lower16:__ccmbss_start
	movt	r0,#:upper16:__ccmbss_start
	movw	r1,#:lower16:__ccmbss_words
	mov	r2,#0
	cbz	r1,1f
0:	str	r2,[r0],#4
	subs	r1,r1,#1
	bne	0b
1:
	bl	_ada_setup_pll
	bl	main

	bl	_exit

hang:   b .

.LinitSp:
	.word __stack_end
