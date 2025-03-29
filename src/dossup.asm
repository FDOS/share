; initializes C library and provides implementation of DOS API calls

%include "segs.inc"


s.text.resident

; not used by resident code, but should not be overwritten by resident data which overwrites .text segment
global keep_
keep_:
;void keep(unsigned retcode, unsigned paras);
	; on entry al=retcode, dx=paras
	mov ah, 0x31
	int 0x21
	; if returns then error
	retn
	


global _end_resident
align 16
_end_resident:	; resident stack starts here


extern _sbss
extern _sstack


s.bss

global __psp
__psp: resw 1

global __environ
__environ: resw 1  ; segment to envirnment block

HEAPSIZE equ 4096
global __heap	; reserved space for all dynamically allocated memory (ensures when loaded [high] enough memory or loading fails)
__heap: resw HEAPSIZE


s.text

extern main_
extern _cstart_
extern amissig.prod

; offsets in PSP
MEMTOP  equ 0x02
ENVIRON equ 0x2C
CMDLEN  equ 0x80 ; 0x80 is length (byte count) of CMDLINE
CMDLINE equ 0x81 ; up to 126 characters, usually terminated with \r


global startup
startup:
	; setup stack
	; although we may compile in small model, we assume CS=DS=SS
	mov ax, cs
	mov ds, ax
	mov es, ax

	; for EXE stack segment is setup, but we want SS=DS
	; we can take difference between SS and DS, adjust SS to DS and increment SP by difference * 16
	; but as this calculation is the same as _sstack + STACK_SIZE, we just set SS:SP directly

	mov bx, OFFSET _sstack
	add bx, STACK_SIZE
	cli
	mov ss, ax
	mov sp, bx
	sti
	
	; TODO validate memory needed is below or equal to MEMTOP paras, ie. SP/16 <= PSP[MEMTOP]

	; we expect rep to increase DI/SI, so clear direction
	cld
	
	; ensure all variables on BSS (uninitialized variables) are 0'data
	; note: because _sstack and _sbss are extern, we can't just compute _sstack - sbss as assemble time constant
	mov di, OFFSET _sbss
	xor ax, ax
	mov cx, OFFSET _sstack
	sub cx, OFFSET _sbss
	shr cx, 1 ; convert from size of BSS in bytes to words
	rep stosw ; clear region
	
	; for development/debugging - clear stack so we can see high mark for stack and heap
	mov di, OFFSET _sstack
	mov ax, 0x90
	mov cx, STACK_SIZE/2
	rep stosw
	
	; save address to PSP for easy access to base segment
	mov ax, cs
	sub ax, 0x10	; com files _psp==cs, for EXE _psp is just prior to cs
	mov [__psp], ax
	mov ds, ax  ; cs=es=ss, ds=PSP=(cs-0x10)
	mov di, OFFSET _sstack  ; es:di points to bottom of stack, where we copy command line with program name to
	
	; copy program name from environment
	mov ax, [ENVIRON]
	mov [es:__environ], ax
	cmp ax, 0
	je .defname ; if no environment, use hard coded name

	; scan environment block for empty env var, word follows with count of strings followed by ASCIIZ program name & path
	mov ds, ax  ; ds points to segment of environment block
	mov si, 0
.findEnvEnd:
	lodsb
	cmp al, 0
	jnz .findEnvEnd
	; found end of environment variable, see if last one
	lodsb
	cmp al, 0
	jnz .findEnvEnd
	; validate at least 1 ASCIIZ string follows
	lodsw
	cmp ax, 0
	jz .defname
	; copy until terminating '\0' found
	xor cx, cx ; see if len(name) > 0
.copyname:
	inc cx
	lodsb
	stosb
	cmp al, 0
	jnz .copyname
	dec di ; back up and overwrite '\0' with space
	cmp cx, 1 ; see if 0 bytes copied, if so use default name
	je .defname
	jmp .copyargs
	
.defname:
	mov ax, cs ; restore DS
	mov ds, ax
	mov si, amissig.prod
	mov cx, 5 ; len("SHARE")
	rep movsb
	
.copyargs:
	; separate name from args with a space
	mov al, ' '
	stosb
	; copy command line to bottom of stack so reachable as near pointer
	mov ax, es:[__psp] ; get PSP segment
	mov ds, ax
	mov cx, [CMDLEN] ; max command line length
	cmp cx, 0
	jz .done
	mov si, CMDLINE  ; offset in PSP of start of command line options
.loop:
	lodsb
	stosb
	or al, al   ; is it '\0'
	jz .done
	cmp al, 0x0D ; is it '\r'
	je .done
	sub cx, 1
	jz .done
	jmp .loop
.done:
	mov al, 0 ; store '\0' terminator
	stosb
	mov ax, cs ; restore ds to same as cs,es, & ss
	mov ds, ax
	
	; setup argc and argv
	call _cstart_
	; exit normally (not resident), assume AL has return from _cstart_
	mov ah, 0x4C
	int 0x21

	
global writechar_
writechar_:
;void writechar(char c);
	mov dl, al
	mov ah, 0x02  ; direct character output
	int 0x21
	retn

global getvect_
getvect_:
;void far * getvect(unsigned char intno);
	push es
	push bx
	mov ah, 0x35 
	int 0x21
	mov ax, bx
	mov dx, es
	pop bx
	pop es
	retn

global setvect_
setvect_:
;void setvect(unsigned char intno, void far * vector);
	; on entry al=intno, mov cx:bx=vector to ds:dx
	push ds
	push dx
	mov ds, cx
	mov dx, bx
	mov ah, 0x25
	int 0x21
	pop dx
	pop ds
	retn
	
global freemem_
freemem_:
;unsigned freemem(unsigned seg);
	push es
	mov es, ax
	mov ah, 0x49
	stc
	int 0x21
	; ax = error code
	pop es
	jc .error
	xor ax, ax
.error:	
	retn

global close_
close_:
;void close(unsigned fileno);
	push bx
	mov bx, ax
	mov ah, 0x3e
	stc
	int 0x21
	pop bx
	jc .error
	xor ax, ax
.error:
	retn
	
global open_
open_:
;int open(const char *filename, int mode, ... );
	; on entry al=access & sharing modes, move bx=filename to ds:dx
	push dx
	mov dx, bx
	mov ah, 0x3d
	int 0x21
	; ax = file handle if carry clear, else error code
	pop dx
	jc .error
	retn
.error:
	mov ax, -1
	retn
	

; implicit functions called by compiler to help with basic mathematical operations

%ifdef __WATCOM__
global _small_code_
_small_code_: ; only here to tell OW linker that yes we expect to be built in small memory model

global __I4M, __U4D ; 32 bit signed multiplication and unsigned division

; multiply cx:bx * dx:ax, result in dx:ax (warning upper 32 bits thrown away including sign -- lower 32bits of __I4M == __U4M)
; AB * CD = (A*C << 32) + ((B*C + A*D) << 16) + B*D where AB, CD are 32 bit unsigned numbers and A,B,C,D are 16 bit unsigned numbers
; cx:bx * dx:ax = (cx*dx << 32) + ((bx*dx + cx*ax) << 16) + bx*ax
; for signed, if negative value, negate value & multiply result by -1
; i.e. sign=0; if cx:bx < 0 then sign=!sign && cx:bx=~(cx:bx)+1; if dx:ax < 0 then sign=!sign && dx:ax=~(cx:bx)+1; if sign then result=~result+1 [times -1]
; LSW = bx * ax (lower 16, save upper 16 in XX)
; MSW = bx * dx + cx * ax + XX (from lsw)
; upper 32 bits is ignored ==> (cx*dx) + upper 16 from MSW calculation
; for OW CX,BX considered destroyed, so we could return full 64bit value in cx:bx:dx:ax
__I4M:
	; calculate sign bit and negate values if needed
	; TODO
__U4M: ; cx:bx * dx:ax, result in dx:ax with high 32bits truncated
%if 0 ; calculate LSW first then MSW
	push si ; temp registers
	push di
	
	mov si, dx ; save dx
	mov di, ax ; save ax
	
	mul bx  ; ax*bx results in dx:ax
	xchg ax, bx ; save LSW in bx  (original bx is now in ax)
	xchg dx, si ; restore dx value and save upper 16 bits XX into si
	mul dx  ; original bx*dx results in dx:ax
	add ax, si ; dx:ax = original (bx*dx) + XX
	adc dx, 0
	mov si, dx  ; save in progress results from dx:ax into si:di
	xchg ax, di ; and restore original ax value
	mul cx  ; original cx*ax results in dx:ax
	add ax, di
	adc dx, si ; dx:ax = original ((bx*dx)+XX) + (cx*ax)
	
	pop si
	pop di

%else ; calculate MSW first then LSW
	push    si

	mov si, bx   ; si = _bx
	xchg bx, ax
	mul dx       ; dx:ax = _dx*_bx , bx = _ax , cx = _cx , si = _bx , _dx lost
	
	xchg cx, ax  ; cx = low(_dx*_bx) , ax = _cx
	mul bx       ; dx:ax = _cx*_ax
	
	add cx, ax   ; cx = low(_cx*ax) + low(_dx*bx) == MSW - XX

	mov ax, si   ; ax=_bx, bx=_ax
	mul bx       ; dx:ax = _bx*_ax , ax = low(_bx*_ax) == LSW , dx = high(_bx*_ax) == XX
	add dx, cx   ; dx = low(_cx*ax) + low(_dx*bx) + high(_bx*_ax) == bx * dx + cx * ax + XX (from lsw)

	pop     si
%endif

	retn


%include "ldivmodu.inc"
__U4D:
                LDIVMODU
%endif

%ifdef gcc
%macro ULONG_HELPERS 1
global %1udivsi3
%1udivsi3:      call %1ldivmodu
                ret 8

global %1umodsi3
%1umodsi3:      call %1ldivmodu
                mov dx, cx
                mov ax, bx
                ret 8

%1ldivmodu:     LDIVMODU

global %1ashlsi3
%1ashlsi3:      LSHLU

global %1lshrsi3
%1lshrsi3:      LSHRU
%endmacro
                ULONG_HELPERS ___
%endif
