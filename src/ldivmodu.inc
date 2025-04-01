; from FreeDOS GPLv2 kernel sources
; divide dx:ax / cx:bx, quotient in dx:ax, remainder in cx:bx

%macro LDIVMODU 0
; this one is adapted from an assembly gem:
; gem writer: Norbert Juffa, norbert.juffa@amd.com

; Dividing 64-bit unsigned integers Assembler / 80386

;   Here is a division routine for dividing two 64-bit unsigned integers. 
;   I derived it by modifying some old
;   16-bit code for dividing 32-bit integers that I did several years ago for a 
;   Turbo-Pascal replacement library.
;   If a 64-bit signed integer division is needed, appropriate shell code for 
;   this routine can easily be written.
;
;   (adapted back to 32-bit by Bart Oldeman ;-))
;
; __U4D divides two unsigned long numbers, the dividend and the divisor
; resulting in a quotient and a remainder.
;
; input:
;   dx:ax = dividend
;   cx:bx = divisor
;%ifdef SIGNEDDIV
;   di = flags for operation requested, div/mod signed/unsigned 
;%endif
;
; output:
;   dx:ax = quotient of division of dividend by divisor
;   cx:bx = remainder of division of dividend by divisor
;
; destroys:
;   flags
;
%ifdef STDCALL
	push bp
	mov bp, sp
	mov ax, [bp+10]	; these offsets assume si & di saved on stack
	mov dx, [bp+12]	; after dividend and divisor pushed on stack
 	mov bx, [bp+14]	; subtrack 4 from offsets if not true 
	mov cx, [bp+16]
	pop bp
%endif

%ifdef SIGNEDDIV
	; di is a flag register, lower 2 bits determine if signed/unsigned and div/mod operation
	; with 3rd bit sign of result

	test di, 1
	jnz .@unsigned
	; if doing signed division, convert to unsigned and save resulting quotient sign
	; i.e. -6/-3=2 -6/3=-2 6/-3=-2 -7/-3=2r1 -7/3=-2r1 7/-3=-2r1
	and di, 3	; flag an even result 0xxb, keep lower 2 bits unchanged
	or dx, dx
	jns .@next		; positive, otherwise convert to positive value (~dx:ax - 1)
	neg dx
	neg ax
	sbb dx, 0
	or di, 4	; 1xxb flag an odd result
.@next:
	or cx, cx
	jns .@unsigned
	neg cx
	neg bx
	sbb cx, 0
	xor di, 4	; ?xxb toggle flag, so 0 if both negative or 1 if only 1 is negative
.@unsigned:
%endif

	test    cx, cx             ; divisor > 2^16-1 ?
	jnz     %%big_divisor      ; yes, divisor > 2^16-1
	cmp     dx, bx             ; only one division needed ? (cx = 0)
	jb      %%one_div          ; yes, one division sufficient


	xchg    cx, ax             ; save dividend-lo in cx, ax=0
	xchg    ax, dx             ; get dividend-hi in ax, dx=0
	div     bx                 ; quotient-hi in ax
	xchg    ax, cx             ; cx = quotient-hi, ax =dividend-lo

%%one_div:    
	div     bx                 ; ax = quotient-lo
	mov     bx, dx             ; bx = remainder-lo
	mov     dx, cx             ; dx = quotient-hi(quotient in dx:ax)
	xor     cx, cx             ; cx = remainder-hi (rem. in cx:bx)
	jmp		%%check_neg

%%big_divisor:
	push    si                 ; save temp
	push    di                 ;  variables
	push    dx                 ; save
	push    ax                 ;  dividend
	mov     si, bx             ; divisor now in
	mov     di, cx             ;  di:si and cx:bx
%%shift_loop:
	shr     dx, 1              ; shift both
	rcr     ax, 1              ;  dividend
	shr     cx, 1              ;   and divisor
	rcr     bx, 1              ;    right by 1 bit
	jnz     %%shift_loop       ;     loop if di non-zero (rcr does not touch ZF)
	div     bx                 ; compute quotient dx:ax>>x / cx:bx>>x (stored in ax; remainder in dx not used)
	pop     bx                 ; get dividend lo-word
	mov     cx, ax             ; save quotient
	mul     di                 ; quotient * divisor hi-word (low only)
	pop     dx                 ; dividend high
	sub     dx,ax              ; dividend high - divisor high * quotient, no overflow (carry/borrow) possible here
	push    dx                 ; save dividend high
	mov     ax, cx             ; ax=quotient
	mul     si                 ; quotient * divisor lo-word
	sub     bx, ax             ; dividend-lo - (quot.*divisor-lo)-lo
	mov     ax, cx             ; get quotient
	pop     cx                 ; restore dividend hi-word
	sbb     cx, dx             ; subtract (divisor-lo * quot.)-hi from dividend-hi
	sbb     dx, dx             ; 0 if remainder > 0, else FFFFFFFFh
	and     si, dx             ; nothing to add
	and     di, dx             ;  back if remainder positive di:si := di:si(cx:bx) & dx:dx
	add     bx, si             ; correct remainder           cx:bx += di:si
	adc     cx, di             ;  and
	add     ax, dx             ;   quotient if necessary           ax += dx
	xor     dx, dx             ; clear hi-word of quot (ax<=FFFFh) dx := 0
	pop     di                 ; restore temp  
	pop     si                 ;  variables
	
%%check_neg:
%ifdef SIGNEDDIV
	test di, 1	; signed operation?
	jnz .@done
	test di, 4	; negative result?
	jnz .@done
	neg dx
	neg ax
	sbb dx, 0
.@done:
%endif
%ifndef STDCALL
	ret
%endif

%endmacro
