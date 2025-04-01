; initializes C library and provides implementation of DOS API calls

%include "segs.inc"


s.text.resident

; not used by resident code, but should not be overwritten by resident data which overwrites .text segment
global _(keep)
_(keep):
;void keep(unsigned retcode, unsigned paras);
	; WATCALL on entry al=retcode, dx=paras
%ifdef STACKCALL
	push bp
	mov bp, sp
	mov ax, bp[4]
	mov dx, bp[6]
	pop bp
%endif
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

extern _(_cstart)
extern amissig.prod

; offsets in PSP
MEMTOP  equ 0x02
ENVIRON equ 0x2C
CMDLEN  equ 0x80 ; 0x80 is length (byte count) of CMDLINE
CMDLINE equ 0x81 ; up to 126 characters, usually terminated with \r

global startup
startup:
	; setup stack
	; compiled in small/tiny model where we assume CS=DS=SS, so setup segment registers to match
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
	call _(_cstart)
	; exit normally (not resident), assume AL has return from _cstart_
	mov ah, 0x4C
	int 0x21

	
global _(writechar)
_(writechar):
;void writechar(char c);
	push dx
%ifdef STACKCALL
	push bp
	mov bp, sp
	mov dx, bp[6]
	pop bp
%else
	mov dl, al
%endif
	mov ah, 0x02  ; direct character output
	int 0x21
	pop dx
	retn

global _(getvect)
_(getvect):
;void far * getvect(unsigned char intno);
	push es
	push bx
%ifdef STACKCALL
	push bp
	mov bp, sp
	mov ax, bp[8]
	pop bp
%endif
	mov ah, 0x35 
	int 0x21
	mov ax, bx
	mov dx, es
	pop bx
	pop es
	retn

global _(setvect)
_(setvect):
;void setvect(unsigned char intno, void far * vector);
	; WATCOM on entry al=intno, mov cx:bx=vector to ds:dx
	; Borland/GCC use stack arguments
	push ds
	push dx
%ifdef WATCALL
	mov ds, cx
	mov dx, bx
%else ; STACKCALL
	push bp
	mov bp, sp
	mov dx, bp[12]
	mov ds, dx
	mov dx, bp[10]
	mov ax, bp[8]
	pop bp
%endif
	mov ah, 0x25
	int 0x21
	pop dx
	pop ds
	retn
	
global _(freemem)
_(freemem):
;unsigned freemem(unsigned seg);
	push es
%ifdef STACKCALL
	push bp
	mov bp, sp
	mov ax, bp[6]
	pop bp
%endif
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

global _(close)
_(close):
;void close(unsigned fileno);
	push bx
%ifdef STACKCALL
	push bp
	mov bp, sp
	mov bx, bp[6]
	pop bp
%else ; WATCALL
	mov bx, ax
%endif
	mov ah, 0x3e
	stc
	int 0x21
	pop bx
	jc .error
	xor ax, ax
.error:
	retn
	
global _(open)
_(open):
;int open(const char *filename, int mode, ... );
	; for WATCALL on entry al=access & sharing modes, move bx=filename to ds:dx
	push dx
%ifdef STACKCALL
	push bp
	mov bp, sp
	mov ax, bp[6]
	mov dx, bp[8]
	pop bp
%else ; WATCALL
	mov dx, bx
%endif
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

%include "lmul.inc"
__I4M:
	; calculate sign bit and negate values if needed
	; TODO
__U4M: ; cx:bx * dx:ax, result in dx:ax with high 32bits truncated
	LMUL

%include "ldivmodu.inc"
__U4D:
	LDIVMODU
%endif


%ifdef __TURBOC__
global N_LXMUL@

%include "lmul.inc"
N_LXMUL@:
	LMUL


global N_LUMOD@, N_LUDIV@
%include "ldivmodu.inc"
	
; N_* are near version, we adjust stack so can far return cs:xx
; F_* are far version, leave asis
N_LDIV@:
	pop ax
	push cs
	push ax
LDIV@:
F_LDIV@:
	xor ax,ax			; 00b for signed division
	jmp short @common

N_LUDIV@:
	pop ax
	push cs
	push ax
LUDIV@:
F_LUDIV@:
	mov ax, 1			; 01b for unsigned division
	jmp short @common

N_LMOD@:
	pop ax
	push cs
	push ax
LMOD@:
F_LMOD@:
	mov ax, 2			; 10b for signed remainder
	jmp short @common

N_LUMOD@:
	pop ax
	push cs
	push ax
LUMOD@:
F_LUMOD@:
	mov ax, 3			; 11b for unsigned remainder
;	jmp short @common

; ax determines operation to perform, moved to di below
; bit 1 determines if signed or unsigned, 
; bit 2 if returns quotient (div) or remainder (mod)
@common:
	push si
	push di
	mov di, ax		; save action to perform
	
	%define STDCALL ; arguments are passed on stack
	%define SIGNEDDIV ; optionally do signed division if (di & 1)
	LDIVMODU		; division with quotient in dx:ax, remainder in cx:bx

	; return results in dx:ax
	test di, 2
	jz @done
	mov dx, cx
	mov ax, bx

@done:
	pop di
	pop si
	retf 8	; also pop our arguments off stack
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
