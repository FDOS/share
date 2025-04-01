; Public Domain (CC0)
; long multiply cx:bx * dx:ax, result in dx:ax with high 32bits truncated

%macro LMUL 0

; multiply cx:bx * dx:ax, result in dx:ax (warning upper 32 bits thrown away including sign -- lower 32bits of __I4M == __U4M)
; AB * CD = (A*C << 32) + ((B*C + A*D) << 16) + B*D where AB, CD are 32 bit unsigned numbers and A,B,C,D are 16 bit unsigned numbers
; cx:bx * dx:ax = (cx*dx << 32) + ((bx*dx + cx*ax) << 16) + bx*ax
; for signed, if negative value, negate value & multiply result by -1
; i.e. sign=0; if cx:bx < 0 then sign=!sign && cx:bx=~(cx:bx)+1; if dx:ax < 0 then sign=!sign && dx:ax=~(cx:bx)+1; if sign then result=~result+1 [times -1]
; LSW = bx * ax (lower 16, save upper 16 in XX)
; MSW = bx * dx + cx * ax + XX (from lsw)
; upper 32 bits is ignored ==> (cx*dx) + upper 16 from MSW calculation
; for OW CX,BX considered destroyed, so we could return full 64bit value in cx:bx:dx:ax
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

%endmacro
