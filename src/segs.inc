; defines segment properties
bits 16
cpu 8086


%define para align=16
%define OFFSET


%ifdef __WATCOM__
group DGROUP _TEXT CONST STRINGS _DATA _TEXT_startup STRINGS_startup CONST_startup _DATA_startup _BSS STACK
; group DGROUP _TEXT CONST STRINGS _DATA DATA XIB XI XIE YIB YI YIE _BSS STACK

 section _TEXT class=CODE          ; resident code
 section CONST class=DATA          ; resident constant data
 section STRINGS class=DATA        ; resident strings data
 section _DATA class=DATA          ; resident initialized data
 section _BSS nobits para class=BSS     ; resident uninitialized data

 section _TEXT_startup class=TCODE ; transient segments
 section CONST_startup class=TDATA
 section STRINGS_startup class=TDATA
 section _DATA_startup class=TDATA

 section STACK nobits para class=STACK  ; transient stack, resident code uses caller's stack

%define s.bss section _BSS
%define s.text section _TEXT
%define s.const section CONST
%define s.stack section STACK
%define s.text.startup section _TEXT_startup
%define s.const.startup section CONST_startup
%define s.data.startup section _DATA_startup
;%else
%define .bss .bss
%define .text .text
%define .stack .stack
%endif


section .text
