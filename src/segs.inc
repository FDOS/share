; defines segment properties
bits 16
cpu 8086


%define para align=16
%define OFFSET
STACK_SIZE      equ     800h


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

%else
%define s.text.resident section .text
%define s.text section .text
%define s.data section .data
%define s.data.resident section .text
%define s.bss section .bss nobits
%define s.stack section .stack nobits
%endif


; default to resident TEXT segment - avoids duplicate warnings with struc/endstruc prior to a segment declaration
s.text.resident
