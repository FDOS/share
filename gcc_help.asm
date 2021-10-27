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


%if 0

Resident code of TSR example
 2020 by C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif

%include "lmacros3.mac"
%include "AMIS.MAC"

%if 0

Supported Int2D functions:

AMIS - Installation check
INP:	al = 00h
OUT:	al = 0FFh
	cx = Private version number (currently 0100h)
	dx:di-> signature: "ecm     ","SHARE   "

AMIS - Get private entry point - NOP: no private entry point
INP:	al = 01h
OUT:	al = 00h

AMIS - Uninstall - NOP: no resident uninstaller or NOP: can't uninstall
INP:	al = 02h
OUT:	If installed from command line,
	 al = 03h
	 bx = memory block of resident TSR (cs)

AMIS - Request pop-up - NOP: no pop-up
INP:	al = 03h
OUT:	al = 00h

AMIS - Determine chained interrupts
INP:	al = 04h
OUT:	al = 04h
	dx:bx-> interrupt hook list (Int2D always.)

AMIS - Get hotkeys - NOP: no hotkeys
INP:	al = 05h
OUT:	al = 00h

AMIS - Get device driver information - NOP: no device
INP:	al = 06h
OUT:	al = 00h

AMIS - Reserved for AMIS
INP:	al = 07h..0Fh
OUT:	al = 00h

TSR - Reserved for TSR
INP:	al = 10h..FFh
OUT:	al = 00h

%endif


	align 2, db 0
amissig:
.ven:	fill 8,32,db "ecm"		; vendor
.prod:	fill 8,32,db "SHARE"		; product
.desc:	asciz "FreeDOS file-sharing and locking capabilities"	; description
%if $ - .desc > 64
 %error AMIS description too long
%endif


amisintr:
.i2F:	db 2Fh
	dw i2F
.i2D:	db 2Dh
	dw i2D


i2D.uninstall:
	inc ax				; (= 03h) safe to remove but no resident uninstaller
	mov bx, cs			; = segment
i2D.hwreset equ $-1		; (second byte of mov bx, cs is same as the retf opcode)
	iret

global i2D_handler
global i2D_next
i2D_handler equ i2D
i2D_next equ i2D.next

iispentry i2D, 0, i2D
	cmp ah, 0
global amisnum
amisnum equ $-1				; AMIS multiplex number (data for cmp opcode)
	je .handle			; our multiplex number -->
	jmp far [cs:.next]		; else go to next handler -->

.handle:
	test al, al
	jz .installationcheck		; installation check -->
	cmp al, 02h
	je .uninstall			; uninstallation -->
	cmp al, 04h
	je .determineinterrupts		; determine hooked interrupts -->
				; all other functions are reserved or not supported by TSR
.nop:
	mov al, 0			; show not implemented
	iret

.installationcheck:
	dec al				; (= FFh) show we're here
	mov cx, 100h			; = version
	mov di, amissig			; dx:di -> AMIS signature strings of this program
.iret_dx_cs:
	mov dx, cs
.iret:
	iret

.determineinterrupts:			; al = 04h, always returns list
	mov bx, amisintr		; dx:bx -> hooked interrupts list
	jmp short .iret_dx_cs


%if 0

End of C. Masloch TSR example code

%endif


extern inner_handler

global handler2f
global old_handler2f
handler2f equ i2F
old_handler2f equ i2F.next

	; IBM Interrupt Sharing Protocol header
iispentry i2F, 0, i2D

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

	; insure UP (Direction Flag = 0) for our C code
	cld

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
	iret

.run_old:
	; jump to old handler that will iret
	jmp far [CS:old_handler2f]
