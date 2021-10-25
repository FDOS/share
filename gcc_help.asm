%define MULTIPLEX_ID 0x10

bits 16

cpu 8086

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
	.flags: resw 1		; (not used by us)

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

extern inner_handler

global handler2f
handler2f:
	; IBM Interrupt Sharing Protocol header
	jmp strict short istart
global old_handler2f
old_handler2f: dd 0
	dw 0x424b	; ("KB")
	db 0		; flag
	jmp strict short hwreset
	times 7 db 0	; pad 7 bytes
istart:
	; Check whether it is a call for us.
	;  Doing this early speeds up the great
	;  majority of cases and allows for some
	;  degree of reentrant calls, assuming
	;  there are not multiple 2F.10 calls,
	;  without messing up our inreentrant
	;  old_ss and old_sp variables.
	cmp ah, MULTIPLEX_ID
	jne .run_old

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
	; don't need IP or CS or flags
	; (flags weren't ever read or written,
	;  and setting flags before iret is pointless.
	;  setting them before chaining is minimally
	;  more useful but not needed here.)

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

	; Although this is a 16-bit variable it is
	;  valid to check only the low 8 bits as
	;  the variable is set either to zero or one.
	; Note that we restore ds between the cmp and
	;  the jnz. This is a small optimisation to
	;  avoid a cs override prefix. The Zero Flag
	;  is unaffected by the mov.
	cmp byte [need_to_chain], 0

	; restore caller's DS
	mov  DS, [iregs.ds]

	; return from interrupt if we handled it
	jnz  .run_old
hwreset: equ $
	iret

.run_old:
	; jump to old handler that will iret
	jmp far [CS:old_handler2f]
