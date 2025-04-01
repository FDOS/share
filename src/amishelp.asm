; contains the helper (transient) logic for multiplex interrupt handling
; (which calls out to C code to actually perform non-AMIS actions)
; and non-resident (transient/startup) code to install/validate/uninstall
; the hooked multiplex interrupt

%if 0  ; based on:

Transient code of TSR example
 2020 by C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif



; DOS SHARE reserved/well known multiplex interupt and id#
%define MUX_INT_NO 0x2f
%define MULTIPLEX_ID 0x10



; standarize segments across C and ASM source
%include "segs.inc"

%include "lmacros3.mac"
%include "amis.mac"


; variables and functions in amisstub.asm that we refer to
struc ctrl1
.offset:	resw 1
.status:	resb 1
.enable:	resb 1
endstruc

extern amissig
extern sft_size

extern clear_sft_shroff

s.data
	align 2, db 0
debuggeramissig:
.ven:	fill 8,32,db "ecm"	; vendor
.prod:	fill 8,32,db "lDebug"	; product

	alignb 2
; 0 if unused, else high byte is debugger AMIS id# and low byte is 0x30, 
; 0x30 is debugger function to execute uninstall AMIS interrupt?
debuggerfunction:	dw 0



; need not remain resident
s.text


; uint16_t impx_install_check(uint16_t multiplex_no, uint16_t multiplex_id) NON_RES_TEXT;
; for int 0x2f and 0x2d install check, al=0, ah=id
global _impx_install_check
_impx_install_check:

	lframe near
	lpar word, mpx_id
	lpar word, mpx_no
	lenter
	push ds
	push es
	push si
	push di
	
	; get int# in al, fixup int # code
	mov ax, word [bp + ?mpx_no]
	mov CS:[OFFSET @the_int + 1], al

	mov ax, word [bp + ?mpx_id]
	xchg al, ah		; ah = multiplex id
	xor  al, al		; al = 0
	
	; skip past hard code debug stuff if not 0x2f
	;cmp word [bp + ?mpx_no], 0x2f
	;jne @the_int
	;mov ax, 1000h	; signature check expects this explicit instruction stream, but only when checking for kernel share flag.
@the_int:
	int MUX_INT_NO	; results in ax

cleanup:
	pop di
	pop si
	pop es
	pop ds
	lleave
	retn


global _asm_find_resident
_asm_find_resident:
	push ds
	push es
	push si
	push di

findinstalled:
	mov ax, 352Dh		; get current interrupt vector 2Dh address
	int 21h
	inc bx
	jz .error			; if ????:FFFF assume invalid
	mov bx, es
	test bx, bx			; or if 0000:???? assume invalid
	jz .error

	push cs
	pop ds
	mov ah, 0FFh		; start with multiplex number 0FFh
.loop:
	call .check
	jnc .end
	sub ah, 1		; search is backward (to find latest installed first), from 0FFh to 00h including
	jnc .loop		; try next if we didn't check all yet -->

		; If not found
.error:
	mov ax, -1		; return code: error, not found
	jmp .ret


		; INP:	ah = multiplex number to check
		;	ds => not yet installed resident segment
		; OUT:	CY if multiplex number unused or no signature match,
		;	 ah, ds unmodified
		;	NC if match found,
		;	 ah = multiplex number (unmodified)
		;	 ds => found already resident segment
		; CHG:	al, si, di, cx, dx
.check:
	mov al, 00h		; AMIS installation check
	int 2Dh			; AMIS (or "DOS reserved" = iret if no AMIS present)
	cmp al, 0FFh
	jne .notfound
	mov si, amissig		; ds:si -> our AMIS name strings
	mov es, dx		; es:di -> name strings of AMIS multiplexer that just answered
	mov cx, 8		; Ignore description, only compare vendor and program name
	repe cmpsw
	jne .notfound		; No match, try next
	db __TEST_IMM8		; (skip stc, NC)

.notfound:
	stc
	retn

.end:
	xchg al, ah
	mov ah, 0		; return code: multiplex number <= 255
.ret:
	pop di
	pop si
	pop es
	pop ds
	retn


	struc status_struct
ssPatchOffset:	resw 1
ssFileSize:	resw 1
ssFileFree:	resw 1
ssLockSize:	resw 1
ssLockFree:	resw 1
ssPatchStatus:	resb 1
ssEnable:	resb 1
ssReserved: resb 4
	endstruc


global _asm_get_status
_asm_get_status:
	lframe near
	lpar word, struct
	lpar word, mpx
	lpar_return
	lenter
	push ds
	push es
	push si
	push di

	mov di, word [bp + ?struct]
	push ds
	pop es			; es:di -> struc passed in
	push di
	mov cx, status_struct_size
	mov al, 0
	rep stosb
	pop di
	mov byte [di + ssEnable], 1

	mov ax, word [bp + ?mpx]
	xchg al, ah		; ah = multiplex number
	test al, al		; is it valid ?
	jnz .ret		; no -->

	mov al, 21h
	int 2Dh
	cmp al, 3
	jb @F
	mov es, dx
	mov dx, word [es:bx]
	mov word [di + ssPatchOffset], dx
				; copy over patch offset
	cmp al, 4
	mov al, byte [es:bx + 2]
	mov byte [di + ssPatchStatus], al
	jb @F
	mov al, byte [es:bx + 3]
	mov byte [di + ssEnable], al
@@:

	mov al, 22h
	push di
	int 2Dh
	cmp al, 8
	jne @F
	push ds
	pop es
	add di, ssFileSize
	mov ds, dx
	mov si, bx
	movsw
	movsw
	movsw
	movsw
	pop di
	jmp @FF

@@:
	pop dx
	xchg dx, di

	cmp al, 0FFh
	jne @F
	mov word [di + ssFileSize], bx
	mov word [di + ssFileFree], cx
	mov word [di + ssLockSize], si
	mov word [di + ssLockFree], dx
@@:

.ret:
	pop di
	pop si
	pop es
	pop ds
	lleave
	lret


global _asm_enable
_asm_enable:

	lframe near
	lpar word, mpx
	lenter
	push ds

	mov ax, word [bp + ?mpx]
	xchg al, ah		; ah = multiplex number
	test al, al		; is it valid ?
	jz @F			; yes -->

		; If not found
	mov ax, 1		; return code: error, 1 (none resident)
	jmp .return

@@:
	mov al, 21h
	int 2Dh
	cmp al, 4
	jb .return_2

	mov ds, dx
	test byte [bx + ctrl1.enable], 1
	jnz .return_3

		; When AMIS function 02h is called, it will
		;  disable the resident handler. However, it
		;  doesn't clear the data. Therefore, when
		;  enabling a disabled resident, clear this.
	push di
	push es
	push dx
	push bx
	call zero_out_data
	pop bx
	pop ds
	pop es
	pop di

	or byte [bx + 3], 1
	xor ax, ax
	jmp .return

.return_3:
	mov ax, 3			; is already enabled
	jmp .return

.return_2:
	mov ax, 2			; ctrl1.enable byte not present
.return:
	pop ds
	lleave
	retn


global _asm_disable
_asm_disable:

	lframe near
	lpar word, mpx
	lenter

	mov ax, word [bp + ?mpx]
	xchg al, ah		; ah = multiplex number
	test al, al		; is it valid ?
	jz @F			; yes -->

		; If not found
	mov ax, 1		; return code: error, 1 (none resident)
	jmp .return

@@:
	mov al, 21h
	int 2Dh
	cmp al, 4
	mov ax, 2
	jb .return
	push ds
	mov ds, dx
	clropt [bx + 3], 1		; disable int 2Fh handler

	push es
	push si
	push di
	push dx
	push bx
	call clear_sft_shroff		; reset SFT field in each SFT
	pop bx
	pop ds

	cmp byte [bx + ctrl1.status], 2
					; patch needed ?
	jne @F
	mov bx, word [bx + ctrl1.offset]
					; get offset
	xor ax, ax
	mov ds, ax			; = 0
	mov ds, word [31h * 4 + 2]	; i31 vector has segment = DOS DS
	mov byte [bx], al		; patch the flag (= 0)
@@:

	mov ah, byte [bp + ?mpx]
	call zero_out_data

	pop di
	pop si
	pop es

	pop ds
	xor ax, ax
.return:
	lleave
	retn


		; INP:	ah = multiplex number
		; CHG:	dx, bx, cx, di, es, ds, ax
zero_out_data:
	mov al, 22h
	int 2Dh
	mov ds, dx		; 22h will return ctrl2 structure pointer in dx:bx, where dx is cs=ds=es=ss of share when ran and segment of corresponding resident section

	cmp al, 4
	jb @F
	push word [bx]
	pop word [bx + 2]

	cmp al, 8
	jb @F
	push word [bx + 4]
	pop word [bx + 6]
@@:

	mov al, 23h
	int 2Dh
	mov ds, dx
	mov es, dx

	cmp al, 4
	jb @F
	xchg ax, dx
	mov di, word [bx]
	mov cx, word [bx + 2]
	xor ax, ax
	rep stosb
	xchg ax, dx

	cmp al, 8
	jb @F
	mov di, word [bx + 4]
	mov cx, word [bx + 6]
	xor ax, ax
	rep stosb
@@:
	retn


global _asm_uninstall
_asm_uninstall:

uninstall:
	push ds
	push es
	push si
	push di
	lframe 2 + 8
	lpar word, mpx
	lenter
	jmp @F

.return:
	lleave code, forcerestoresp
	pop di
	pop si
	pop es
	pop ds
	retn

	; from here to uninstall_get_mpx does an AMIS check for debugger with AMIS functionality we use to do actual interrupt unhooking
	; sets debuggerfunction to either 0 (not found/suppported) or AMIS id# and function code 0x30
@@:
findinstalleddebugger:
	push cs
	pop ds
	mov ah, 0FFh		; start with multiplex number 0FFh
.loop:
	call .check
	mov al, 30h		; al = 30h to indicate found, ah = multiplex number
	jnc .end
	sub ah, 1		; search is backward (to find latest installed first), from 0FFh to 00h including
	jnc .loop		; try next if we didn't check all yet -->

	xor ax, ax		; al = 0 to indicate none found
	jmp .end		;  If not found, continue -->


		; INP:	ah = multiplex number to check
		;	ds = ss = cs
		; OUT:	CY if multiplex number unused or no signature match,
		;	 bp, ah, ds unmodified
		;	NC if match found,
		;	 ah = multiplex number (unmodified)
		; CHG:	si, di, es, cx, dx
.check:
	mov al, 00h		; AMIS installation check
	int 2Dh			; AMIS (or "DOS reserved" = iret if no AMIS present)
	cmp al, 0FFh
	jne .notfound
	mov si, debuggeramissig	; ds:si -> our AMIS name strings
	mov es, dx		; es:di -> name strings of AMIS multiplexer that just answered
	mov cx, 8		; Ignore description, only compare vendor and program name
	repe cmpsw
	je .checkret		; ZR, NC = match -->
.notfound:
	stc			; NZ, CY no match
.checkret:
	retn

.end:
	mov word [debuggerfunction], ax

uninstall_get_mpx:
	mov ax, word [bp + ?mpx]
	xchg al, ah		; ah = multiplex number
	test al, al		; is it valid ?
	jz @F			; yes -->

		; If not found
	mov ax, 1		; return code: error, 1 (none resident)
	jmp uninstall.return

@@:

uninstall_found_installed:	; ah = AMIS multiplex number of resident copy

		; Check first if interrupts can be unhooked.
		;  If not, we want to stay resident and enabled.
	mov al, 04h
	mov bl, 2Dh			; Dummy, for the API. We only accept code 04h.
	int 2Dh				; AMIS determine hooked interrupts
	cmp al, 04h
	jne .fail			; General uninstallers should be prepared for at least 04h or 03h
						; as return code here.
	push ax
	mov ds, dx			; ds:bx-> interrupt table
	mov dx, UnhookInterruptSim
	mov cx, .simulatedbefore
	jmp loopamisintr
.simulatedbefore:
	jc unhookerror
	pop ax				; retain multiplex number

	mov al, 02h
	mov dx, cs
	mov bx, .done			; dx:bx = return address if successful
	int 2Dh				; AMIS uninstall TSR
	cmp al, 0FFh			; 0FFh successful
	je .done			; TSR has already done everything -->
	test al, al			; 00h not implemented
	jz .continue_noseg		; do it myself -->
	cmp al, 03h			; 03h safe, no resident uninstaller (still enabled). bx=segment
	je .continue_seg		; expected if !_SUPPORTGENERALUNINSTALLER -->
	cmp al, 04h			; 04h safe, no resident uninstaller (now disabled). bx=segment
	je .continue_seg		; expected if _SUPPORTGENERALUNINSTALLER, but continue -->
		; (other values: 01h unsuccessful or internal, 02h can't uninstall yet,
		; but will do so when able, 05h not safe, 06h,07h device driver; all fail)
.fail:
	mov ax, 2			; return code: error, 2 (some failure)
	jmp uninstall.return


.continue_noseg:
	xor bx, bx			; = 0

.continue_seg:
	push bx

	mov al, 04h
	mov bl, 2Dh			; Dummy, for the API. We only accept code 04h.
	int 2Dh				; AMIS determine hooked interrupts
	cmp al, 04h
	jne .fail		; General uninstallers should be prepared for at least 04h or 03h
				; as return code here.
	mov ds, dx			; ds:bx-> interrupt table
	push bx
	mov dx, UnhookInterruptSim
	mov cx, .simulated
	jmp loopamisintr
.simulated:
	jc unhookerror
	pop bx				; restore ds:bx->

	mov dx, UnhookInterrupt
	mov cx, .unhooked
	jmp loopamisintr
.unhooked:
	jc unhookerrorcritical

	push ds
	pop es				; es = segment of interrupt list
	pop ax				; stacked segment
	test ax, ax			; marker to use interrupt list segment ? (zero)
	jz .uselistsegment		;  yes -->
	mov es, ax			; else use segment returned by uninstall function

.uselistsegment:
	mov ah, 49h			; Free memory
	int 21h
; General deinstallation code should not assume the TSR was just in
; that single memory block. If the deinstallation code doesn't know
; the TSR, it should search (and if it finds any, free) memory blocks
; that have the MCB owner either that's now in es (code segment of TSR)
; or the same MCB owner value that the TSR's code segment's MCB had.
; (Only if the TSR's code segment was a valid memory block with MCB.)
	jc .fail			; if that causes an error, still report "fail"
					; (but interrupts are already unhooked now)

.done:
	call clear_sft_shroff		; do this if resident did not yet

	xor ax, ax			; return code: 0, success
@@:
	jmp uninstall.return

unhookerror:
	mov ax, 3		; return code: error, 3 (unhook simulatiom failed)
	jmp @B

unhookerrorcritical:
	mov ax, 4		; return code: error, 4 (unhook failed)
	jmp @B

	lleave ctx


global _asm_init
_asm_init:
	push ds
	push es
	push si
	push di

	mov dx, 3Bh			; default SFT entry size
	mov ax, 1216h
	xor bx, bx			; SFT handle 0
	stc
	int 2Fh				; es:di -> SFT entry
	jc .gotsize
	test bx, bx			; bx = relative entry-in-table
	jnz .gotsize			; if unexpected (nonzero) -->
	mov cx, di			; remember offset
	push es				; remember segment
	mov ax, 1216h
	inc bx				; = SFT entry 1
	stc
	int 2Fh
	pop ax				; restore segment
	jc .gotsize
	dec bx				; expect 1
	jnz .gotsize			; wasn't 1 -->
	sub di, cx			; = size of entry
	mov cx, es
	cmp cx, ax			; segment matches ?
	jne .gotsize			; if no -->
	mov dx, di			; take computed size
.gotsize:
	mov word [cs:sft_size], dx

	xor ax, ax			; return code is an uint16_t (unused)

	pop di
	pop si
	pop es
	pop ds
	retn


		; INP:	ds:bx -> AMIS interrupt list
		;	dx = code called for each interrupt (with ds:si-> interrupt entry, al = interrupt number)
		;	cx = code to return to
		; OUT:	CY if any interrupt failed,
		;	 ax = first failed interrupt
		;	 ss:sp-> 0FFFFh terminated list of failed interrupts on stack
		;	NC if all interrupts successfully uninstalled
		; CHG:	si, ax, bx, what called code (dx) changes
		;
		; Called code (dx) has to preserve dx, bx, cx, al.
loopamisintr:

	mov ax, 0FFFFh
	push ax
.loop:
	mov al, byte [ bx ]
	mov si, word [ bx+1 ]

	call dx
	jnc .noerror			; no error -->
	xor ah, ah
	push ax				; else remember number of interrupt, but continue looping

	; cmp dx, HookInterrupt
	; je .abort			; if during hooking (!!), abort now -->

.noerror:
	add bx, byte 3
	cmp al, 2Dh
	jne .loop			; do until 2Dh done -->

.abort:
	pop ax
	cmp ax, byte -1			; if it's below (CY), there were errors
	jmp cx

%if 0
		; INP:	es:bx-> interrupt entry
		; OUT:	NZ if non-IISP entry,
		;	 or IISP entry that doesn't chain
		;	ZR if IISP entry that chains
IsChainingIISPEntry?:
	test byte [ es:bx + ieEOI ], 80h	; this one a non-chaining handler ? (or non-IISP)
	jnz IsIISPEntry?.return			; yes -->
						; otherwise fall through to check if really an IISP entry
%endif

		; INP:	es:bx-> interrupt entry
		; OUT:	NZ if non-IISP entry
		;	ZR if IISP entry
IsIISPEntry?:
	cmp bx, - (ieSignature + 2)		; may access word at offset FFFFh ?
	ja .return				; yes, avoid --> (NZ)
	cmp word [ es:bx + ieSignature ], "KB"	; "KB"/424Bh ? ("BK" in MASM)
	jne .return
	cmp word [ es:bx + ieEntry ], 0EA90h	; nop\jmp far imm16:imm16 ?
	je .return				; unused IISP entry (created by iHPFS) -->
	cmp byte [ es:bx + ieEntry ], 0EBh	; jmp short ... ?
		; (This opcode should strictly be jmp short $+18 but there's programs
		; that save an additional jmp opcode by jumping directly into their
		; code even though it's not right behind the header.)
	jne .return
	cmp byte [ es:bx + ieJmphwreset ], 0EBh	; jmp short ... ?
	je .return				; usual IISP entry -->
	cmp byte [ es:bx + ieJmphwreset ], 0CBh	; retf ?
	je .return				; a shorter variant -->
	cmp byte [ es:bx + ieJmphwreset ], 0CFh	; iret ?
.return:
	retn


		; INP:	ds:si -> source IISP header (or pseudo header)
		;	es:di -> destination IISP header
		; OUT:	EI
		;	si and di both incremented by 6
		; CHG:	-
		; STT:	UP
update_iisp_header:
	push ax
	mov ax, word [cs:debuggerfunction]
	test ax, ax			; found the debugger ?
	jz @F				; no -->
	int 2Dh				; call its Update IISP Header function
	cmp al, 0FFh			; supported ?
	pop ax
	je .ret				; yes. done -->
	db __TEST_IMM8			; (skip pop)
@@:
	pop ax				; restore ax, then do manual update
	cli				; try to rest while updating chain
	cmpsw				; skip over first word (entrypoint)
					;  (generally xxEBh or 0EA90h)
	movsw
	movsw				; transfer source ieNext to dest ieNext
	sti
.ret:
	retn


		; INP:	al = interrupt number
		;	ds:si-> interrupt entry
		; OUT:	CY if unhooking failed
		;	NC if unhooking successful
		; CHG:	ah, es, di, si
global UnhookInterrupt
UnhookInterrupt:
			; UnhookInterruptSim (below) only checks if it's possible to unhook this interrupt.
			; This function really unhooks the interrupt if possible.
			;
			; This is to cover the situation when some of the hooked interrupts can unhook,
			; but some can't. If the uninstaller would start to unhook the interrupts and then
			; catch the interrupt that can't be unhooked the user would end up with a dead TSR
			; that's uninstalled halfway. Very bad.
			;
			; "Simulating" the unhooking first and checking if all interrupts can unhook
			; usually will not return such a state.
	call UnhookInterruptSim
	jc .ret				; bad. --> (CY)
	jz .easy
.hard:
				; "hard" case: UnhookInterruptSim has however already done the work,
				; so the hard case is here indeed easier than the easy case.
	call update_iisp_header		; copies our stored pointer into the other's entry
	clc
	retn
.easy:
	push ds
	push dx
	lds dx, [ si + 2 ]		; get what we stored in the entry
	mov ah, 25h			; easy case - just reset to the value stored
	int 21h				; doesn't alter CF (leaves NC from UnhookInterruptSim) or sets NC
	pop dx
	pop ds
.ret:
	retn

		; INP:	ds:si-> IISP entry
		;	al = interrupt number
		; OUT:	NC if no error (either hard or easy case),
		;	 ZR if easy case,
		;	  ds:si-> our IISP entry, containing stored interrupt
		;	 NZ if hard case,
		;	  ds:si-> our IISP entry
		;	  es:di-> IISP entry to modify
		;	CY if error (not first handler and no IISP chain to this handler)
		; CHG:	ah, es, di
global UnhookInterruptSim
UnhookInterruptSim:
	push bx

	; harden this, check we are an IISP entry
	 push ds
	 pop es				; es => our handler segment
	mov bx, si			; es:bx -> our handler
	call IsIISPEntry?		; does it have an IISP header ?
	jne .fail			; fail if not

	mov ah, 35h			; get current vector
	int 21h				; es:bx-> current interrupt handler
	cmp si, bx			; our pointer ?
	jne .hard

	push ax
	push si
	mov si, ds
	mov ax, es
	 cmp si, ax			; our segment ?
	pop si
	pop ax
	jne .hard

	and ah, 00h			; NC, ZR
	pop bx
	retn
.hard:
		; INP:	ds:si-> IISP entry
		;	es:bx-> current interrupt entry
		; OUT:	CY if error
		;	NC, NZ if no error,
		;	 ds:si-> our IISP entry
		;	 es:di-> IISP entry to modify
		; CHG:	ah, es, di, (bx)
	call SearchIISPChain
	jne .harder
.found:				; found reference to our interrupt handler
	mov di, bx			; es:di-> IISP entry that references our's
	or ah, 0FFh			; NC, NZ
	pop bx
	retn

.harder:			; Desperate attempt to find IISP entry that references ours by
				; searching through the interrupts hooked by other AMIS TSRs. Note
				; that the plexer loop will find and search through the list of
				; hooked interrupts of the uninstalling TSR itself, but this causes
				; no trouble.
		; INP:	ds:si-> IISP entry
		; OUT:	CY if error
		;	NC, NZ if no error,
		;	 ds:si-> our IISP entry
		;	 es:di-> IISP entry to modify
		; CHG:	ah, es, di, (bx)
	push dx
	push ax				; register with interrupt number last
	xor ax, ax
.loopplex:
	mov al, 00h			; AMIS installation check
	push cx
	int 2Dh				; enquire whether there's anyone
	pop cx				;  but we don't care who it might be
	inc al
	jz .search
.nextplex:
	inc ah
	jnz .loopplex			; try next multiplexer -->
	pop ax
	pop dx
.fail:					; IISP incompatible TSR between current interrupt entry and our entry
					;  and no AMIS compatible TSR installed on top of our entry
	stc
	pop bx
	retn

		; INP:	ah = multiplex number of AMIS TSR to search through
		;	ss:sp-> interrupt number (byte), must be preserved
		; CHG:	es, di, dx, bx
.search:
	mov al, 04h
	pop bx
	push bx				; low byte is the interrupt number
	int 2Dh
	cmp al, 03h			; returned its interrupt entry ?
				; RBIL doesn't explicitly state that this interrupt entry has to
				; be IISP compatible. But I'm too lazy to look up the older AMIS,
				; and SearchIISPChain checks the interrupt entry anyway.
	je .search_dxbx
	cmp al, 04h			; returned list of hooked interrupts ?
	jne .nextplex			; no, try next multiplexer -->
	mov di, bx
	pop bx
	push bx				; bl = interrupt number
	mov al, bl
.search_intlist_seg:
	mov es, dx			; es:di-> list
.search_intlist:		; Search the returned list for the required interrupt number.
	scasb				; our interrupt number ?
	je .search_found_intlist
	cmp byte [es:di-1], 2Dh		; was last in list ?
	je .nextplex
	scasw				; skip pointer
	jmp short .search_intlist	; try next entry -->
.search_found_intlist:
	mov bx, word [es:di]		; dx:bx-> IISP entry
	scasw				; skip pointer
	call SearchIISPChain
	je .search_found		; found entry -->
		; This specific jump supports TSRs that hook the same
		; interrupt more than once; jumping to .nextplex instead
		; (as previously) aborts the search after the first match
		; in the interrupt list. This support might become useful.
	cmp al, 2Dh			; was last in list ?
	je .nextplex
	jmp short .search_intlist_seg

.search_dxbx:
	mov es, dx			; es:bx-> (IISP) interrupt entry
				; The entry we found now is possibly behind the non-IISP entry that
				; terminated our first SearchIISPChain call (at .hard). We then
				; possibly might find our entry in this hidden part of the chain.
	call SearchIISPChain
	jne .nextplex			; didn't find our entry in the chain -->
.search_found:
	pop ax
	pop dx
	jmp short .found


SearchIISPChain.next:
	les bx, [es:bx +2]		; get next interrupt entry

		; INP:	ds:si-> IISP entry
		;	es:bx-> current interrupt entry
		; OUT:	NZ if reference to ds:si not found in IISP chain es:bx->
		;	ZR if reference found,
		;	 es:bx-> IISP (or uninstalled iHPFS) interrupt entry with reference
		; CHG:	es, bx
SearchIISPChain:
	call IsIISPEntry?			; that an IISP entry ?
	jnz .return				; nope --> (NZ)
	cmp si, word [ es:bx + ieNext ]		; our pointer ?
	jne .next				; no, try next -->
	push ax
	mov ax, ds
	cmp ax, word [ es:bx + ieNext + 2]	; our segment ?
	pop ax
	jne .next				; no, try next -->
.return:					; yes, found (ZR)
	retn


%if 0

End of C. Masloch TSR example code

%endif

