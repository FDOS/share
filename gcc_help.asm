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
	dx:di-> signature: "DOS-C   ","SHARE   "

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
INP:	al = 10h..20h
OUT:	al = 00h

SHARE - Get patch status (ctrl1)
INP:	al = 21h
OUT:	al = size of returned data (not 0 if supported, 3 for now)
	dx:bx -> patch offset word, then patch status byte

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

TSR - Reserved for TSR
INP:	al = 23h..FFh
OUT:	al = 00h

%endif


	align 2, db 0
amissig:
.ven:	fill 8,32,db "DOS-C"		; vendor
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


	; These variables are used as globals
	;  by the C code. Each is an uint16_t.
global file_table_size
global file_table_free
global lock_table_size
global lock_table_free

	align 2, db 0
ctrl2:
file_table_size:	dw 0
file_table_free:	dw 0
lock_table_size:	dw 20
lock_table_free:	dw 20
ctrl2.end:

ctrl1:
.offset:	dw -1
.status:	db 1
.end:


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
	cmp al, 21h
	je .ctrl1
	cmp al, 22h
	je .ctrl2
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

.ctrl1:
	mov al, ctrl1.end - ctrl1
	mov bx, ctrl1
	jmp short .iret_dx_cs

.ctrl2:
	mov al, ctrl2.end - ctrl2
	mov bx, ctrl2
	jmp short .iret_dx_cs


section .text.startup

global asm_find_resident
asm_find_resident:
	push ds
	push es
	push si
	push di

findinstalled:
	mov ax, 352Dh
	int 21h
	inc bx
	jz .error
	mov bx, es
	test bx, bx
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
	endstruc


global asm_get_status
asm_get_status:
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
	mov al, byte [es:bx + 2]
	mov byte [di + ssPatchStatus], al
@@:

	mov al, 22h
	push di
	int 2Dh
	cmp al, 8
	jne @F
	pop di
	push ds
	pop es
	add di, ssFileSize
	mov ds, dx
	mov si, bx
	movsw
	movsw
	movsw
	movsw
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


global asm_uninstall
asm_uninstall:

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
	mov al, 02h
	mov dx, cs
	mov bx, .done			; dx:bx = return address if successful
	int 2Dh				; AMIS uninstall TSR
	cmp al, 0FFh			; 0FFh successful
	je .done			; TSR has already done everything -->
	test al, al			; 00h not implemented
	jz .continue_noseg		; do it myself -->
	cmp al, 03h			; 03h safe, no resident uninstaller (still enabled). bx=segment
	je .continue_seg		; expected -->
	cmp al, 04h			; 04h safe, no resident uninstaller (now disabled). bx=segment
	je .continue_seg		; unexpected, but continue -->
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

clear_sft_shroff:
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
	add di, dx			; es:bx + di -> next entry, if any
	loop .entryloop
.tablenext:
	les bx, [es:bx]			; -> next table, bx = -1 if none
	jmp .tableloop

.done:

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


section .data.startup
	align 2, db 0
debuggeramissig:
.ven:	fill 8,32,db "ecm"	; vendor
.prod:	fill 8,32,db "lDebug"	; product

section .bss.startup nobits
	alignb 2
debuggerfunction:
		resw 1				; = 0 if unused

%if 0

End of C. Masloch TSR example code

%endif


section .text

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

	push ds

	; set our data segment
	push CS
	pop DS

	test al, al
	jz install_check

.enter_c_handler:

	; save the input DS
	pop word [iregs.ds]

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


	numdef CMPSTR, 1

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
	mov al, 0C3h
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
