; contains the resident logic for multiplex interrupt handling
; (which calls out to C code to actually perform non-AMIS actions)
; and non-resident (transient/startup) code to install/validate/uninstall
; the hooked multiplex interrupt

%if 0  ; based on:

Resident code of TSR example
 2020 by C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif



; DOS SHARE reserved/well known multiplex interupt and id#
%define MUX_INT_NO 0x2f
%define MULTIPLEX_ID 0x10



%if 0

Alternate Multiplex Interrupt Supported Int2D functions:

AMIS - Installation check
INP:	al = 00h
OUT:	al = 0FFh
	cx = Private version number (currently 010Ah)
	dx:di-> signature: "DOS-C   ","SHARE   "

AMIS - Get private entry point - NOP: no private entry point
INP:	al = 01h
OUT:	al = 00h

AMIS - Uninstall - NOP: no resident uninstaller
INP:	al = 02h
OUT:	if _SUPPORTGENERALUNINSTALLER=1,
	 al = 04h (safe to remove, no resident uninstaller, TSR now disabled)
	 bx = memory block of resident TSR (cs)
	 patch applied. SFT fields reset. int 2Fh handler disabled
	else,
	 al = 03h (safe to remove, no resident uninstaller, TSR still enabled)
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
INP:	al = 10h..20h
OUT:	al = 00h

SHARE - Get [FreeDOS kernel share enabled] patch status (ctrl1)
INP:	al = 21h
OUT:	al = size of returned data (not 0 if supported, 4 for now)
	dx:bx -> patch offset word, then patch status byte,
		then enable byte

SHARE - Get data on free and total structures
INP:	al = 22h
OUT:	al = FFh if supported without table,
	 bx = file table total size (amount entries)
	 cx = file table free amount entries
	 si = lock table total size (amount entries)
	 di = lock table free amount entries
	al = 8 if supported with table,
	 dx:bx -> table of 4 words,
	 file total, file free, lock total, lock free

SHARE - Get data on structure pointers
INP:	al = 23h
	al = (N * 4) if supported,
	 dx:bx -> table (word offset in same segment, word size)

TSR - Reserved for TSR
INP:	al = 24h..FFh
OUT:	al = 00h

%endif



; standarize segments across C and ASM source
%include "segs.inc"

%include "lmacros3.mac"
%include "amis.mac"

	numdef SUPPORTGENERALUNINSTALLER, 1



s.data.resident

global _iregs
_iregs:
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

global _need_to_chain
_need_to_chain:
	resw 1

global _top_of_stack
_top_of_stack:
	resw 1

old_ss:
	resw 1
old_sp:
	resw 1


	align 2, db 0
global amissig
global amissig.prod
amissig:
.ven:	fill 8,32,db "DOS-C"		; vendor
.prod:	fill 8,32,db "SHARE"		; product
.desc:	asciz "FreeDOS file-sharing and locking capabilities"	; description
%if $ - .desc > 64
 %error AMIS description too long
%endif


amisintr:	; AMIS list of hooked interrupts, 2Dh marks end of list
.i2F:	db 2Fh
	dw i2F
.i2D:	db 2Dh
	dw i2D


	; These variables are used as globals
	;  by the C code. Each is an uint16_t.
global _file_table_size
global _file_table_size_bytes
global _file_table_free
global _file_table ;_offset
global _lock_table_size
global _lock_table_size_bytes
global _lock_table_free
global _lock_table ;_offset

extern _end_resident


	align 2, db 0
ctrl2:
_file_table_size:	dw 0
_file_table_free:	dw 0
_lock_table_size:	dw 20
_lock_table_free:	dw 20
ctrl2.end:

	align 2, db 0
ctrl3:
_file_table:
_file_table_offset:	dw OFFSET _end_resident	; WARNING: this will overwrite startup/transient code
_file_table_size_bytes:	dw 0
_lock_table:
_lock_table_offset:	dw 0
_lock_table_size_bytes:	dw 0
ctrl3.end:

ctrl1:
.offset:	dw -1
.status:	db 1
.enable:	db 1
.end:


; see sharetsr.c
; This table determines the action to take when attempting to open a file.
; static unsigned char open_actions[5][5] = { ... };
global _open_actions:
_open_actions: 
    db 0, 1, 1, 1, 1
    db 2, 1, 1, 1, 1
    db 2, 1, 1, 1, 1
    db 2, 1, 1, 1, 1
    db 2, 1, 1, 1, 0

; and exceptions to above table
; static open_action_exception_t open_exceptions[] = { ... };
global _open_exceptions
_open_exceptions:
    db 0, 0, 2, 0, 3
    db 0, 0, 4, 0, 3  ;	/* compatibility-read/deny none-read, MED 08/2004 */
    db 2, 0, 0, 0, 4
    db 2, 0, 2, 0, 0
    db 2, 0, 4, 0, 0
    db 3, 0, 2, 1, 0
    db 3, 0, 4, 1, 0
    db 3, 1, 4, 1, 0
    db 3, 2, 4, 1, 0
    db 4, 0, 0, 0, 4
    db 4, 0, 0, 1, 0  ;	/* deny none-read/compatibility-write */
    db 4, 0, 0, 2, 0  ;	/* deny none-read/compatibility-read+write */
    db 4, 0, 2, 0, 0
    db 4, 0, 2, 1, 0
    db 4, 0, 2, 2, 0
    db 4, 1, 3, 0, 0
    db 4, 1, 3, 1, 0
    db 4, 1, 3, 2, 0


; can be placed in transient section if not _SUPPORTGENERALUNINSTALLER
global sft_size
sft_size:	dw 0


s.text.resident

; invoke DOS critical error handler for given drive & error code
; void critical_error(uint16_t error_code, uint16_t drive) __gcc16;
global _critical_error
_critical_error:

	lframe near
	lpar word, error_code
	lpar word, drive
	lenter

	mov ax, word [bp + ?drive]
	mov di, word [bp + ?error_code]
	mov ah, 0eh ; disk I/O; fail allowed; data area
	int 24h

	lleave
	retn


i2D.uninstall:
%if _SUPPORTGENERALUNINSTALLER
	mov al, 04h		; safe to remove, no resident uninstaller,
					;  TSR now disabled
	push es
	push ds
	push si
	push di
	push ax
	push cx
	push dx

		; This code helps a general uninstaller that is not aware of
		;  our specific situation. So, disable our handler, clear
		;  the SFT field, and do the patch to the flag if needed.
		; Our own uninstaller can check the patch status and its
		;  flag contents before and after calling the uninstall
		;  function to determine if we patched it already. Its
		;  code still allows to finish up removal for any build of
		;  the resident, regardless of what it does include.
	cld

	push cs
	pop ds
	clropt [ctrl1.enable], 1	; first disable the TSR

	call clear_sft_shroff		; reset SFT field in each SFT

	cmp byte [ctrl1.status], 2	; patch needed ?
	jne @F
	mov bx, word [ctrl1.offset]	; get offset
	xor ax, ax
	mov ds, ax			; = 0
	mov ds, word [31h * 4 + 2]	; i31 vector has segment = DOS DS
	mov byte [bx], al		; patch the flag (= 0)
@@:

	pop dx
	pop cx
	pop ax
	pop di
	pop si
	pop ds
	pop es
%else
	inc ax				; (= 03h) safe to remove but no resident uninstaller
%endif
	mov bx, cs			; = segment
i2D.hwreset equ $-1		; (second byte of mov bx, cs is same as the retf opcode)
	sub bx, 10h			; memory block to dealloc is PSP, so subtract sizeof(PSP) from CS
	iret

global _i2D_handler
global _i2D_next
_i2D_handler equ i2D
_i2D_next equ i2D.next

iispentry i2D, 0, i2D
	cmp ah, 0
global _amisnum
_amisnum equ $-1				; AMIS multiplex number (data for cmp opcode)
	je .handle					; our multiplex number -->
	jmp far [cs:.next]			; else go to next handler -->

.handle:
	test al, al
	jz .installationcheck		; installation check -->
	cmp al, 02h
	je .uninstall				; uninstallation -->
	cmp al, 04h
	je .determineinterrupts		; determine hooked interrupts -->
	cmp al, 21h
	je .ctrl1
	cmp al, 22h
	je .ctrl2
	cmp al, 23h
	je .ctrl3
				; all other functions are reserved or not supported by TSR
.nop:
	mov al, 0					; show not implemented
	iret

.installationcheck:
	dec al						; (= FFh) show we're here
	mov cx, 10Ah				; = version
	mov di, amissig				; dx:di -> AMIS signature strings of this program
.iret_dx_cs:
	mov dx, cs
.iret:
	iret

.determineinterrupts:			; al = 04h, always returns list
	mov bx, amisintr			; dx:bx -> hooked interrupts list
	jmp short .iret_dx_cs

.ctrl1:
	mov al, ctrl1.end - ctrl1
	mov bx, ctrl1
	jmp short .iret_dx_cs

.ctrl2:
	mov al, ctrl2.end - ctrl2
	mov bx, ctrl2
	jmp short .iret_dx_cs

.ctrl3:
	mov al, ctrl3.end - ctrl3
	mov bx, ctrl3
	jmp short .iret_dx_cs


; *** logic not required by resident code moved to amishelp.asm

%if 0

End of C. Masloch TSR example code

%endif


s.text.resident

extern _(inner_handler)

global _handler2f
global _old_handler2f
_handler2f equ i2F
_old_handler2f equ i2F.next

	; IBM Interrupt Sharing Protocol header
iispentry i2F, 0    ; *** can use i2D for 3rd argument but exceeds jmp short

	; see if our handler is disabled, 
	; if so skip it and go straight to chaining old interrupt handler
	test byte [cs:ctrl1.enable], 1
	jz .run_old

	; Check whether it is a call for us.
	;  Doing this early speeds up the great
	;  majority of cases and allows for some
	;  degree of reentrant calls, assuming
	;  there are not multiple 2F.10 calls,
	;  without messing up our inreentrant
	;  old_ss and old_sp variables.
	cmp ah, MULTIPLEX_ID
	jne .run_old

	push ds

	; set our data segment
	push CS
	pop DS

	test al, al
	jz install_check

.enter_c_handler:

	; save the input DS
	pop word [_iregs.ds]

	; save regs
	mov  [_iregs.bp], BP
	mov  [_iregs.di], DI
	mov  [_iregs.si], SI
	mov  [_iregs.es], ES
	mov  [_iregs.dx], DX
	mov  [_iregs.cx], CX
	mov  [_iregs.bx], BX
	mov  [_iregs.ax], AX
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
	mov SP, [_top_of_stack]

	; insure UP (Direction Flag = 0) for our C code
	cld

	; call our handler
	call _(inner_handler)

	; restore stack
	mov SS, [old_ss]
	mov SP, [old_sp]

	; restore register input values
	mov  BP, [_iregs.bp]
	mov  DI, [_iregs.di]
	mov  SI, [_iregs.si]
	mov  ES, [_iregs.es]
	mov  DX, [_iregs.dx]
	mov  CX, [_iregs.cx]
	mov  BX, [_iregs.bx]
	mov  AX, [_iregs.ax]

	; Although this is a 16-bit variable it is
	;  valid to check only the low 8 bits as
	;  the variable is set either to zero or one.
	; Note that we restore ds between the cmp and
	;  the jnz. This is a small optimisation to
	;  avoid a cs override prefix. The Zero Flag
	;  is unaffected by the mov.
	cmp byte [_need_to_chain], 0

	; restore caller's DS
	mov  DS, [_iregs.ds]

	; return from interrupt if we handled it
	jnz  .run_old
	iret

.run_old:
	; jump to old handler that will iret
	jmp far [CS:_old_handler2f]


	numdef CMPSTR, 1

; we are looking for share_installed byte flag in FreeDOS dosfns.c
; specifically, function IsShareInstalled function updates this variable
; in older versions, it was initialized to 0 and once share installed
; would be set to nonzero, it was never reset to 0 if share subsequently 
; uninstalled, so we follow call chain, if originates in kernel and from
; function share_check() in int2f.asm as called by IsShareInstalled() in
; dosfns.c then we retrieve offset to share_installed variable from 
; IsShareInstalled stack frame.  We later clear or set as share is 
; installed and uninstalled.  The status variable is thus indeterminate
; until the kernel makes its share installed check.
; kernel has since been updated to update share_installed on its own,
; as such does not need patching for current 2044+ or newer kernels and
; this code can be removed & just call into c_handler if not supporting older kernels.
install_check:
	cmp byte [ctrl1.status], 1		; still indeterminate ?
	jne i2F.enter_c_handler			; no -->
	lframe 0
	lpar word, nearip			; kernel retn frame
	lpar word, intflags			; iret frame
	lpar dword, intcsip			; iret frame
	lpar word, ds				; on stack
	lenter
	push es
	push di
	push si
	push cx
	les di, [bp + ?intcsip]
%ifn _CMPSTR
	cmp di, -16				; insure scasw is ok
	ja .ret
%endif
	mov si, seq_call_i2F
	mov cx, seq_call_i2F.length
	sub di, cx
	repe cmpsb
	jne .ret
	push di
%ifn _CMPSTR
	mov al, __TEST_IMM16
	scasb
	jne @F
	mov ax, "US"				; Uninstallable SHARE signature
	scasw
@@:
%else
	mov si, seq_signature
	mov cl, seq_signature.length
	repe cmpsb
%endif
	pop di
	jne .check_needed
	mov byte [ctrl1.status], 3		; not needed
	jmp .ret

.check_needed:
	mov al, 0C3h			; ret instruction immediately follows our signature?
	scasb					; expected ?
	jne .ret
	mov di, word [bp + ?nearip]
	cmp di, -16				; insure mov ax is fine
	ja .ret
	mov si, seq_setting
	mov cl, seq_setting.length
	mov ax, [es:di + seq_setting.offset - seq_setting]
						; get their offset, if any
	mov word [si + seq_setting.offset - seq_setting], ax
						; plug it into the sequence
	repe cmpsb				; need patch ?
	jne .ret				; don't know -->
	mov es, cx				; = 0
	mov si, word [bp + ?ds]			; verify ds
	cmp si, word [es:31h * 4 + 2]		; i31 vector has segment = DOS DS
	jne .ret
	mov word [ctrl1.offset], ax		; remember offset
	inc byte [ctrl1.status]			; = 2, patch needed
.ret:
	pop cx
	pop si
	pop di
	pop es
	lleave
	mov ax, 1000h
	jmp i2F.enter_c_handler


seq_call_i2F:
.:	mov ax, 1000h
	int 2Fh
.length: equ $ - .
%if _CMPSTR
seq_signature:
.:	test ax, "US"				; Uninstallable SHARE signature
.length: equ $ - .
%endif
seq_setting:
.:	cmp al, 0FFh
	jnz .not
	mov byte [0], 1
.offset: equ $ - 3
.not:
.length: equ $ - .


%if _SUPPORTGENERALUNINSTALLER
s.text.resident
%else
s.text
%endif

global clear_sft_shroff
clear_sft_shroff:

; Refer to https://github.com/FDOS/kernel/blob/cedcaee5adbc2d0d4d08c3572aae8decf50d4bb4/kernel/dosfns.c#L538
;  and the structure at https://github.com/FDOS/kernel/blob/cedcaee5adbc2d0d4d08c3572aae8decf50d4bb4/hdr/sft.h#L79
; Absent SHARE the record number (word at 33h) is initialised as -1.
;  So we overwrite the record number with this for every file.
;  (Redirectors might use the SHARE record number field to their
;  own purposes so don't modify their entries.)

	mov ah, 52h
	int 21h
	les bx, [es:bx + 4]		; -> first SFT table
.tableloop:
	cmp bx, -1			; was last table ?
	je .done			; yes -->
	mov di, 6			; es:bx + di -> first entry
	mov cx, word [es:bx + 4]	; amount of entries this table
	jcxz .tablenext
.entryloop:
	cmp word [es:bx + di], 0	; referenced ?
	je .entrynext			; no -->
	testopt [es:bx + di + 5], 8080h, 1
					; redirector or character device ?
	jnz .entrynext			; yes, skip -->
	or word [es:bx + di + 33h], -1	; reset sharing record number
.entrynext:
	add di, word [cs:sft_size]	; es:bx + di -> next entry, if any
	loop .entryloop
.tablenext:
	les bx, [es:bx]			; -> next table, bx = -1 if none
	jmp .tableloop

.done:
	retn
