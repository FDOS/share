bits 16

cpu 8086

section .tsr_text

extern __lnon_resident_text
extern __edata
extern __snon_resident_text
extern _start

global tsr_startup
tsr_startup:
	pushf
	push SI
	push DI
	push CX

	; copy text out from .bss
	mov CX, __lnon_resident_text
	mov SI, __edata
	mov DI, __snon_resident_text
	cld
	rep movsb

	pop CX
	pop DI
	pop SI
	popf

	; jump to standard startup code
	jmp _start

section .bss

global iregs
iregs:
	.bp: resw 1
	.di: resw 1
	.si: resw 1
	.ds: resw 1
	.es: resw 1
	.dx: resw 1
	.cx: resw 1
	.bx: resw 1
	.ax: resw 1
	.ip: resw 1
	.cs: resw 1
	.flags: resw 1

global need_to_chain
need_to_chain:
	resw 1

global top_of_stack
top_of_stack:
	resw 1

old_ss:
	resw 1
old_sp:
	resw 1

section .text

extern old_handler2f
extern inner_handler

global handler2f
handler2f:
	; save the input DS
	mov  [CS:iregs.ds], DS

	; set our data segment
	push CS
	pop DS

	; save regs
	mov  [iregs.bp], BP
	mov  [iregs.di], DI
	mov  [iregs.si], SI
	mov  [iregs.es], ES
	mov  [iregs.dx], DX
	mov  [iregs.cx], CX
	mov  [iregs.bx], BX
	mov  [iregs.ax], AX
	; don't need IP or CS
	pushf
	pop word [iregs.flags]

	; setup stack
	mov [old_ss], SS
	mov [old_sp], SP
	push CS
	pop SS
	mov SP, [top_of_stack]

	; call our handler
	call inner_handler

	; restore stack
	mov SS, [old_ss]
	mov SP, [old_sp]

	; restore register input values
	mov  BP, [iregs.bp]
	mov  DI, [iregs.di]
	mov  SI, [iregs.si]
	mov  ES, [iregs.es]
	mov  DX, [iregs.dx]
	mov  CX, [iregs.cx]
	mov  BX, [iregs.bx]
	mov  AX, [iregs.ax]

	push word [iregs.flags]
	popf

	; restore caller's DS
	mov  DS, [iregs.ds]

	; return from interrupt if we handled it
	cmp  word [CS:need_to_chain], 0
	jnz  .run_old
	iret

.run_old:
	; jump to old handler that will iret
	push word [CS:old_handler2f + 2]
	push word [CS:old_handler2f]
	retf
