; this is the first object compiled/linked and provides layout for program


; layout during initialization / program run
; group DGROUP _TEXT CONST _DATA _BSS STACK
; layout while resident
; group DGROUP _TEXT STACK  <-- we cut _TEXT into lower resident and upper nonresident pieces, so only some of _TEXT still exists
; Warning: any data that is used by resident code must be placed in _TEXT below _end_resident

; jmp to init code (text)
; resident variables (data)
; AMIS logic (text)
; share logic (text)
; init code (text_startup)
; init data and const (const_startup, data_startup)
; init uninitialized variables (bss)
; init stack


%include "segs.inc"

s.text.resident ; start of s.text
extern startup
global entry	; entry point to pass to Watcom's WLINK
entry:
..start:		; entry in OBJ for implicit entry point for OptLink
jmp startup
db "FD SHARE", 26,0

; resident variables are stored in TEXT segment

;s.text

s.data

s.bss
global _sbss
_sbss: ; start of BSS

s.stack
global _sstack
_sstack:   ; end of BSS (start of STACK)
	dw (STACK_SIZE) dup (?)
