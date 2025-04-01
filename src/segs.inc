; defines segment properties
bits 16
cpu 8086


%define para align=16
%define OFFSET
STACK_SIZE      equ     800h


; all functions default to one of the following calling conventions
; for all, any returned value is in AL, AX, DX:AX as appropriate for size unless otherwise stated
; however, pointers may be returned in SI for WATCALL, BX for FASCALL/MSFASTCALL (no pointers returned, to verify???)
; except for PASCAL, any argument not passed in register is passed on stack right to left with caller cleans stack
; unless otherwise specified we assume AX,BX,CX,DX may be modified with all other registers preserved (ES may be modified by Watcom)
; volatile registers (assumed can be modified without saving): ax, cx, dx
; CSTACKCALL (_cdecl) - all arguments pushed on stack right to left, caller cleans stack
; WATCALL - up to 1st 4 int arguments passed in registers AX, DX, BX, CX
;           or 2 long arguments in registers DX:AX, CX:BX
;           Note: AL uses AX, longs only passed in DX:AX or CX:BX so char,long uses AL & CX:BX with DX unused.
;           and variadic functions (...) are passed same as _cdecl (all args on stack)
; FASTCALL (_fastcall) - (Borland version) up to 1st 3 int arguments passed in registers AX, DX, CX (not used)
;           Note: gcc with regparm=3/regparmcall is similar except callee cleans stack
; MSFASTCALL (_msfastcall / _fastcall for MSC and gcc) - up to 1st 2 int arguments passed in registers CX, DX (not used)
; STDCALL (_stdcall) - args are passed same as _cdecl (on stack) but callee cleans stack like _pascal (not used)
; PASCALCALL (_pascal) - all arguments pushed on stack left to right, callee cleans stack  (not used)


%ifdef __WATCOM__
group DGROUP _TEXT CONST CONST2 STRINGS _DATA _BSS STACK
;group DGROUP _TEXT CONST CONST2 STRINGS _DATA DATA XIB XI XIE YIB YI YIE _BSS STACK

 section _TEXT class=CODE     ; code and resident data
 section CONST class=DATA
 section CONST2 class=DATA
 section STRINGS class=DATA
 section _DATA class=DATA
 section _BSS nobits para class=BSS
 section STACK nobits para class=STACK  ; transient stack, resident code stack overwrites _TEXT

%define s.text.resident section _TEXT
%define s.text section _TEXT
%define s.const section CONST
%define s.data section _DATA
%define s.data.resident section _TEXT
%define s.bss section _BSS
%define s.stack section STACK

; we use Watcom register passing argument convention
%define WATCALL
; external functions are wrapped in _(name) to prepend/append _ as appropriate for compiler
%define _(name) name %+ _

%ifdef UNUSED
%macro _extern 1
extern _%1
%endmacro
%macro _extern_fn 1
extern %1_
%endmacro
%macro _global 1
global _%1
_%1:
%endmacro
%macro _global_fn 1
global %1_
%1_:
%endmacro
%endif
%endif


%ifdef __TURBOC__
group DGROUP _TEXT CONST CONST2 STRINGS _DATA _BSS STACK

 section _TEXT class=CODE     ; code and resident data
 section CONST class=DATA
 section CONST2 class=DATA
 section STRINGS class=DATA
 section _DATA class=DATA
 ; note Borland compilers may place BSS after STACK, we place bss prior to stack
 section _BSS nobits para class=BSS
 section STACK nobits para class=STACK  ; transient stack, resident code stack overwrites _TEXT
 
%define s.text.resident section _TEXT
%define s.text section _TEXT
%define s.const section CONST
%define s.data section _DATA
%define s.data.resident section _TEXT
%define s.bss section _BSS
%define s.stack section STACK

; external functions are wrapped in _(name) to prepend/append _ as appropriate for compiler
%define _(name) _ %+ name
%endif


%ifdef gcc
%define s.text.resident section .text
%define s.text section .text
%define s.data section .data
%define s.data.resident section .text
%define s.bss section .bss nobits
%define s.stack section .stack nobits

; external functions are wrapped in _(name) to prepend/append _ as appropriate for compiler
%define _(name) name
%endif


%ifndef WATCALL
; assume C calling convention with arguments passed on stack right to left
%define STACKCALL
%endif


; default to resident TEXT segment - avoids duplicate warnings with struc/endstruc prior to a segment declaration
s.text.resident
