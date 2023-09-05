; Forth system for Commander X16 - port of Forth Model T
; by Vasyl Tsvirkunov
; At this point the compliance status is:
; Forth-2012 System
; Providing the Core Extensions word set
; Providing names from the Programming-Tools word set
; Providing names from the String word set
; Providing the Double-Number word set
; Providing names from the File-Access word set (note that Model T had full compliancy) 
; Providing names from the Facility word set
; 
; This system will pass all standard tests for Core, Core Extensions, and Double-Number.
; The supplied subset of Facility word set is sufficient to pass that test completely (only five words are tested).
; The partial Programming-Tools and String wordsets are compliant and will also pass individual tests.
; With dynamic-memory-allocation package by Ulrich Hoffmann installed the implementation will pass the
; Memory-Allocation tests (the implementation is slightly non-compliant and fails on negative sizes
; passed to ALLOCATE and RESIZE, those need to be patched for 100% clean test).
; Optional Block, Exception, Locals, and Search-Order sets are not implemented. No Floating-Point either.

; Significant changes compared to Forth Model T
; I/O functionality is limited according to the platform constraints
; TOUPPER has been removed as it does not make much sense with PETSCII. All string constants brought to uppercase
; Removed CSTR, not used
; Added PLACE and +PLACE
; A number of words were rewritten in assembly for speed

; Peculiarities:
;	C64 PETSCII charset does not have backslash. Pound symbol is used instead
;	It does not have tilde either. Not used in standard words, but it is used in test suite

; C64/Commander X16 prolog. "1 SYS 2061"
; Make sure the file is built in CBM mode (extra $01,$08 at the beginning)

* = $0801
start_of_image:
    !byte $0b,$08,$01,$00,$9e,$32,$30,$36,$31,$00,$00,$00

; I/O entries in KERNAL
SETLFS = $FFBA
SETNAM = $FFBD
OPEN = $FFC0
CLOSE = $FFC3
CHKIN = $FFC6
CHKOUT = $FFC9
CLRCHN = $FFCC
CHRIN = $FFCF
CHROUT = $FFD2
GETIN = $FFE4
READST = $FFB7
STOP = $FFE1
UDTIM = $FFEA
SCNKEY = $FF9F

!convtab raw

; ==============================================================================
; Definitions - constants and variables. Unlike the original RatVM, C64 has some
; memory reserved for ROMs, some zero page locations for registers, etc. In short,
; more realistic case. Forth Model T assumes contiguous chunk of RAM, but the top
; location is configurable and the bottom is not assumed, which can be made
; compatible with X16 mmap.
; Per documentation: RAM goes $0000-$9EFF, but the PRG has to load above $801 (see the prolog).
; That gives 38K, not a whole lot, but enough for a competent Forth system.
; Locations $22-$7F on zero page are available, using those for variables.
; The stacks are a bit of pain. The thing is, 6502 is not really an 8-bit CPU, there
; were not that many truly 8-bit ones - that would restrict RAM to 256 bytes. So, the
; RAM bus is 16 bit, which is good, but the registers are all 8-bit, and that makes
; things difficult. The standard stack is only 256 bytes, not great for Forth (but
; many implementations use that for data stack anyway). For this implementation, we will
; get two 1K stacks - return stack at the top of the RAM and data stack at $0400-$07FF.
; However, to make implementation more efficient, the very top element of the data stack
; is separated on the zero page. The safe areas against overflow/underflow are 4 cells vs
; 8 cells in Forth Model T. Return stack does not use safe areas, as it is rarely an issue
; (the system is likely to crash and burn at that point already).

SSAFE = 4		; Number of reserved cells on the both sides of data stack for protection
RSIZE = $0400	; Return stack in bytes
DSIZE = $0400	; Data stack in bytes

; In case if these get moved - important requirement, stacks must be aligned on 16-bit boundary
RSTACK = $9f00 - RSIZE
RSTACK_INIT = RSTACK + RSIZE - 2
DSTACK = $0400
DSTACK_INIT = DSTACK + DSIZE - 2*SSAFE - 2

STACKLIMIT = DSIZE - 4*SSAFE

NAMEMASK = 31 ; vocabulary entries can have up to 32 characters
IMM_FLAG = 128 ; flag for immediate words
NST_FLAG = 64 ; flag for words not in ANS Forth
VAL_TRUE = -1 ; required by current standard
VAL_FALSE = 0
JSR_INSTR = $20 ; DOES> needs to emit JSR opcode

NEW_LINE = $0D

USER_BASE	= $22
_ri			= USER_BASE		; inner interpreter registers
_w			= _ri+2
_rstack		= _w+2			; stack pointers
_dstack		= _rstack+2
_dtop		= _dstack+2		; the very top element of the data stack
_rscratch	= _dtop+2		; three scratch registers used in multiple algorithms (sometimes aliased)
_wscratch	= _rscratch+2
_scratch	= _wscratch+2

; Other zero page vars
_here		= _scratch+2
_base		= _here+2
_context	= _base+2
_latest		= _context+2
_state		= _latest+2
_strict		= _state+2
_sflip		= _strict+2		; flip-flop for S" buffer selection
_ibufcount	= _sflip+2		;number of used buffers
_source 	= _ibufcount+2 ; pointer to the current source
_openfiles	= _source+2 	; bitfield for files currently open to translate from C64 to Forth opening semantics

_stopcheck	= _openfiles+2
; And here's the spot to move the inner interpreter. There is more than enough space to fit that here
next 		= _stopcheck+1

; Some commonly used 6502 idioms. Note that ACME does not allow passing immediate values to macros...

!macro add .op {
	clc
	adc #.op
}

!macro sub .op {
	sec
	sbc #.op
}

!macro ldax .addr {
	lda .addr
	ldx .addr+1
}

!macro stax .addr {
	sta .addr
	stx .addr+1
}

; Small increment on A/X
!macro incax .op {
	clc
	adc #.op
	bcc +
	inx
+
}

!macro decax .op {
	sec
	sbc #.op
	bcs +
	dex
+
}

; Stack access is behind macros so it is easier to change in the future

!macro rpush {
	jsr push_rstack
}

!macro rpop {
	jsr pop_rstack
}

!macro dpush {
	jsr push_dstack
}

!macro dpop {
	jsr pop_dstack
}

!macro init_rstack {
	lda #<RSTACK_INIT
	ldx #>RSTACK_INIT
	+stax _rstack
}

!macro init_dstack {
	lda #<DSTACK_INIT
	ldx #>DSTACK_INIT
	+stax _dstack
}

; Miscellaneous buffers allocated below the top of the accessible memory
_ibuf = RSTACK - 700 		; seven 100-char buffers for INCLUDE-FILE
_sbuf = _ibuf - 200 		; two 100-char buffers for S" / S\"
_fnamebuf = _sbuf - 100 	; buffer for filename storage only

_tib = _fnamebuf - 100		; input buffer (reserving 100 bytes although only 81 are really needed)
_wordbuf = _tib - 100 		; buffer to hold result of WORD (reserving 100 bytes)
_findbuf = _wordbuf - 100	; used by FIND to keep case-converted pattern word
_pad = _findbuf - 100		; PAD buffer
_hld = _pad - 100			; pointer for pictured numeric output, the 98-byte buffer follow
_hldend = _pad
_sourcestack = _hld 		; end of the stack for sources (120 bytes to accomodate 7 files and default)

MEMTOP = _sourcestack - 120

; ==============================================================================

; To make the system more robust we need some way to recover from stack
; underruns and overruns. It cannot be 100% foolproof (the system with
; unrestricted POKE cannot be foolproof) but a single DROP should not
; take out the entire system.
; So, the approach is the following: reserve first four and last four words
; of the stack and call ABORT if checks detect the stack there.

	+ldax xinit_startup		; init mutable system values (in high memory)
	+stax _ri				; _w does not need to be initialized

	+init_rstack
	+init_dstack

	ldx #0
	txa
	+stax _state
	+stax _strict
	+stax _sflip
	+stax _ibufcount

	sta _tib
	sta _wordbuf

	lda #10
	+stax _base

	lda #7			; do not try to open 0-2
	+stax _openfiles

	+ldax xinit_context
	+stax _context
	+ldax xinit_latest
	+stax _latest
	+ldax xinit_here
	+stax _here

; ==============================================================================

; Structure of a vocabulary word in indirect threaded code:
; offset     length      meaning
;    0          1        n - length of the name and flags (NFA)
;    1          n        name
;    n+1        2        link to the previous word (LFA)
;    n+3        2        pointer to code (typically CALL) (CFA)
;    n+5        x        parameter list (for CALL these often are pointers
;                          to code fields of other words (PFA)
; Note that the CPU architecture does not require alignment. Otherwise it would
; be easy to align LFA
; Example:
; Forth   : Example 2 dup + . ;
; Assembly:
; example_n: !byte 6
;            !text "Example"
;            !word last_n
; example:   !word call
;            !word lit, 2, dup, plus, dot, exit

; Move the inner interpreter to zeropage. The only reason for that is to save a few
; clocks on the workaround for broken indirect JMP
	ldy #call-next_orig
	lda #<next_orig
	sta _rscratch
	lda #>next_orig
	sta _rscratch+1
	lda #next
	sta _wscratch
	lda #0
	sta _wscratch+1
inner_copy:
	lda (_rscratch),y
	sta (_wscratch),y
	dey
	bpl inner_copy
	jmp next

; ==============================================================================
; Inner interpreter
;
; To start the interpreter, RI needs to point to CFA of the first word to execute and NEXT should be executed

; NEXT - execute the word at RI (RI is pointer to CFA), prime the parameter pointer for CALL
;	W = mem(RI)
;	RI += 2
;	goto mem(W)

_jmpw = next + jmpw_orig + 1 - next_orig
jmponw = next + jmponw_orig - next_orig

next_orig:
	ldy #0
	lda (_ri),y
	sta _w
	iny
	lda (_ri),y
	sta _w+1
	
	lda _ri
	+add 2
	sta _ri
	bcc jmponw_orig
	inc _ri+1

jmponw_orig:
	ldy #1
	lda (_w),y
	sta <(_jmpw+1)
	dey
	lda (_w),y
	sta <_jmpw
jmpw_orig:
	jmp $FFFF	; This command will be modified on zeropage


; CALL - this will execute the parameters at W
; 	rpush(RI)
;	RI = W+2
;	goto NEXT

call:
; STOP key handler will trigger on every 256th execution of call
	inc _stopcheck
	bne call_continue
	jsr STOP
	bne call_continue
	jmp abort_c
call_continue:

	+ldax _ri
	+rpush
	+ldax _w
	+incax 2
	+stax _ri
	jmp next

; EXIT - return from the current word to the caller (called "return" here to avoid conflict with EXIT word)
;	RI = rpop()
;	goto NEXT

return:
	+rpop
	+stax _ri
	jmp next

; INVOKE - this will execute word by CFA and continue to the next word (exposed to the language as EXECUTE)
;	W = pop()
;	goto mem(W)

invoke:
	+dpop
	+stax _w
	jmp jmponw

; CREATED - push the PFA on the stack (default semantics of a word after CREATE)
;	push(W+2)
;	goto NEXT

created:
	+ldax _w
	+incax 2
	+dpush
	jmp next

; DOES - semantics applied to the defined word by DOES>. It is an extension of CREATE semantics that redirects
; the execution to the creating word
;	rpush(RI)
;	RI = pop()   - this is supposed to be the return address from the SUB (JSR) instruction, not the top of the Forth stack!
;	push(W+2)
;	goto NEXT

does:
	+ldax _ri
	+rpush
	
	pla
	+add 1		; courtesy of 6502 JSR instruction, the return address is off by one
	sta _ri
	pla
	adc #0
	sta _ri+1
	
	jmp created	; starting with PUSH(W+2) it is the same

; DODEFER - semantics of the word created by DEFER
;	W = mem(W+2)
;	goto mem(W)

dodefer:
	ldy #3
	lda (_w),y
	tax
	dey
	lda (_w),y
	+stax _w
	jmp jmponw

; DOVALUE - semantics of a VALUE
;	Assumes a very particular structure: pointer to semantics block followed by value bytes. Semantics block contains
;	three addresses: read semantics, write semantics, compilation semantics
;	push(W+4)
;	W = mem(mem(W+2))
;	goto mem(W)

dovalue:
	+ldax _w
	+incax 4
	+dpush
	ldy #3
	lda (_w),y
	sta _rscratch+1
	dey
	lda (_w),y
	sta _rscratch
	dey
	lda (_rscratch),y
	sta _w+1
	dey
	lda (_rscratch),y
	sta _w
	jmp jmponw

; A number of Forth words have constant semantics. Typical systems define CONSTANT using DOES> but that wastes a few
; bytes for the call. Using a separate semantic word instead.
;	push mem(W+2)
;	goto NEXT

doconst:
	ldy #3
	lda (_w),y
	tax
	dey
	lda (_w),y
	+dpush
	jmp next

; Return stack and data stack implementations, using correspondingly _rstack and _dstack pointers.
; Note that the data stack has the topmost item stored separately in _dtop

push_rstack:
	ldy #0
	sta (_rstack),y
	iny
	txa
	sta (_rstack),y
	lda _rstack
	bne push_rstack_1
	dec _rstack+1
push_rstack_1:
	dec _rstack
	dec _rstack
	rts
	
pop_rstack:
	ldy #2
	lda (_rstack),y
	inc _rstack
	pha
	lda (_rstack),y
	tax
	pla
	inc _rstack
	bne pop_rstack_1
	inc _rstack+1
pop_rstack_1:
	rts

push_dstack:
	pha
	ldy #0
	lda _dtop
	sta (_dstack),y
	iny
	lda _dtop+1
	sta (_dstack),y
	pla
	+stax _dtop
	lda _dstack
	bne push_dstack_1
	dec _dstack+1
push_dstack_1:
	dec _dstack
	dec _dstack
	rts

pop_dstack:
	+ldax _dtop
	pha
	ldy #2
	lda (_dstack),y
	sta _dtop
	inc _dstack
	lda (_dstack),y
	sta _dtop+1
	inc _dstack
	bne pop_dstack_1
	inc _dstack+1
pop_dstack_1:
	pla
	rts

; Other generally useful routines
; Move memory _scratch bytes from _rscratch to _wscratch (_wscratch < _rscratch, otherwise propagation occurs)
movedown:
	ldy #0
	ldx _scratch+1
	beq movedown_2
movedown_1:
	lda (_rscratch),y
	sta (_wscratch),y
	iny
	bne movedown_1
	inc _rscratch+1
	inc _wscratch+1
	dex
	bne movedown_1
movedown_2:
	ldx _scratch
	beq movedown_4
movedown_3:
	lda (_rscratch),y
	sta (_wscratch),y
	iny
	dex
	bne movedown_3
movedown_4:
	rts

; Similarly, move memory when _wscratch > _rscratch
moveup:
	ldx _scratch+1
	txa
	clc
	adc _rscratch+1
	sta _rscratch+1
	txa
	clc
	adc _wscratch+1
	sta _wscratch+1
	inx
	ldy _scratch
	beq moveup_3
	dey
	beq moveup_2
moveup_1:
	lda (_rscratch),y
	sta (_wscratch),y
	dey
	bne moveup_1
moveup_2:
	lda (_rscratch),y
	sta (_wscratch),y
moveup_3:
	dey
	dec _rscratch+1
	dec _wscratch+1
	dex
	bne moveup_1
	rts

; ==============================================================================
; This word became standard in ANS Forth, part of optional Programming-Tools word set. Quit the interpreter.

;
; code bye
; code exit
; code execute
; : quit (sst) ;code
; : abort begin depth 0> while drop again begin depth <0 while 0 again quit ;
;

bye_n:
	!byte 3
	!text "BYE"
	!word 0
bye:
	!word bye_c
bye_c:
	rts

; EXIT is used to return from any word.
exit_n:
	!byte 4
	!text "EXIT"
	!word bye_n
exit:
	!word return

; Execute the word by address on the stack
execute_n:
	!byte 7
	!text "EXECUTE"
	!word exit_n
execute:
	!word invoke

; Reset return stack, dispose of sources, close all open files, and reenter the system.
quit_n:
	!byte 4
	!text "QUIT"
	!word execute_n
quit:
	!word call, xsst, xquit
xquit:
	!word quit_c				; this is an equivalent to ;CODE
quit_c:
	jsr CLRCHN
	lda #7			; do not try to open 0-2
	sta _openfiles
	lda #0
	sta _openfiles+1
	
	+init_rstack
	lda #<forth_system_r		; don't show the banner
	ldx #>forth_system_r
	+stax _ri
	jmp next

; Reset data stack and perform QUIT.
abort_n:
	!byte 5
	!text "ABORT"
	!word quit_n
abort:
	!word abort_c
abort_c:
	+init_dstack
	jmp quit_c

;
; : (sst) _sourcestack
;         2- 0 over ! 2- _tib over ! 2- 0 over ! 2- 0 over ! 2- 4 over !
;         _source ! 0 dup _sflip ! _ibufcount ! ; nonstandard
;

xsst_n:
	!byte 5 + NST_FLAG
	!text "(SST)"	; Reset source stack
	!word abort_n
xsst:
	!word call, lit, _sourcestack
	!word twominus, zero, over, poke			; #TIB
	!word twominus, lit, _tib, over, poke		; TIB
	!word twominus, zero, over, poke			; >IN
	!word twominus, zero, over, poke			; SOURCE-ID
	!word twominus, lit, $0004, over, poke		; standard input has 4 parameters: 0, >IN, TIB, #TIB
	!word lit, _source, poke
	!word zero, dup, lit, _sflip, poke, lit, _ibufcount, poke, exit

; Support for creating of a new executable binary. The (INITS) contains values that may change from the
; original binary (we intentionally don't modify anything below the original end of image). SAVE-SYSTEM
; first writes everything before the body of that word, then constructs a small buffer with different
; values, writes it out, and writes past that to HERE.
;
; code (inits) nonstandard
; : save-system parse-name w/o openfile 0= if
;               >r 0 $xinit_startup r@ write-file drop
;               pad $forth_system_c over !
;               2+ here over ! 2+ _context @ over ! 2+ _latest @ over !
;               drop pad 8 r@ write-file drop
;               $xinit_latest 2+ here over - r@ write-file drop r> close-file
;               then drop ; nonstandard
;

xinits_n:
	!byte 7 + NST_FLAG
	!text "(INITS)"
	!word xsst_n
xinits:
	!word created
xinit_startup:
	!word forth_system_c
xinit_here:
	!word end_of_image
xinit_context:
	!word forth_system_n
xinit_latest:
	!word forth_system_n

; SAVE-SYSTEM has been modified for X16 version - the image start is not 0
; anymore and the image requires the image start to be written to the file first
savesystem_n:
	!byte 11 + NST_FLAG
	!text "SAVE-SYSTEM"
	!word xinits_n
savesystem:
	!word call, parsename, wo, openfile, zeroeq, qbranch, savesystem_1
	!word tor
	!word lit, start_of_image, pad, poke, pad, two, rat, writefile
	!word lit, start_of_image, lit, xinit_startup, over, sub, rat, writefile, drop
	!word pad, lit, forth_system_c, over, poke
	!word twoplus, here, over, poke
	!word twoplus, lit, _context, peek, over, poke
	!word twoplus, lit, _latest, peek, over, poke
	!word drop, pad, lit, $0008, rat, writefile, drop
	!word lit, xinit_latest, twoplus, here, over, sub, rat, writefile, drop
	!word rfrom, closefile
savesystem_1:
	!word drop, exit


; ==============================================================================
; Integer math

; Adding special constants for small numbers that are used very often. This makes both interpretation
; and execution a little bit more efficient.
;
; 0 constant 0
; 1 constant 1
; 2 constant 2
; -1 constant -1
;

zero_n:
	!byte 1 + NST_FLAG
	!text "0"
	!word savesystem_n
zero:
	!word doconst, $0000

one_n:
	!byte 1 + NST_FLAG
	!text "1"
	!word zero_n
one:
	!word doconst, $0001

two_n:
	!byte 1 + NST_FLAG
	!text "2"
	!word one_n
two:
	!word doconst, $0002

minusone_n:
	!byte 2 + NST_FLAG
	!text "-1"
	!word two_n
minusone:
	!word doconst, $FFFF

; The alternative high-level implementations are particularly useful on
; platforms without native multiplication or division.
;
; code +
; code -
; code *        alt: : * m* drop ;
; code /        alt: : / /mod nip ;
; code mod      alt: : mod /mod drop ;
; code /mod     alt: : /mod >r s>d r> sm/rem ;
; code */mod    alt: : */mod >r m* r> sm/rem ;
; code */       alt: : */ */mod nip ;
;

add_n:
	!byte 1
	!text "+"
	!word minusone_n
add:
	!word add_c
add_c:
	+dpop
	clc
	adc _dtop
	sta _dtop
	txa
	adc _dtop+1
	sta _dtop+1
	jmp next

sub_n:
	!byte 1
	!text "-"
	!word add_n
sub:
	!word sub_c
sub_c:
	+dpop
	+stax _rscratch
	lda _dtop
	sec
	sbc _rscratch
	sta _dtop
	lda _dtop+1
	sbc _rscratch+1
	sta _dtop+1
	jmp next

mult_n:
	!byte 1
	!text "*"
	!word sub_n
mult:
	!word call, mmult, drop, exit
		
div_n:
	!byte 1
	!text "/"
	!word mult_n
div:
	!word call, divmod, nip, exit

mod_n:
	!byte 3
	!text "MOD"
	!word div_n
mod:
	!word call, divmod, drop, exit

divmod_n:
	!byte 4
	!text "/MOD"
	!word mod_n
divmod:
	!word call, tor, stod, rfrom, smrem, exit

multdivmod_n:
	!byte 5
	!text "*/MOD"
	!word divmod_n
multdivmod:
	!word call, tor, mmult, rfrom, smrem, exit

multdiv_n:
	!byte 2
	!text "*/"
	!word multdivmod_n
multdiv:
	!word call, multdivmod, nip, exit

;
; code abs		alt: abs dup 0< if negate then ;
; code negate
; code 1+		alt:	: 1+ 1 + ;
; code 1-		alt:	: 1- 1 - ;
; code 2+		alt:	: 2+ 2 + ;
; code 2-		alt:	: 2- 2 - ;
; code 2/		alt:	: 2/ 2 / ;
; code 2*		alt:	: 2* 2 * ;
;

abs_n:
	!byte 3
	!text "ABS"
	!word multdiv_n
abs:
	!word abs_c
abs_c:
	lda _dtop+1
	bmi negate_c
	jmp next
	!word call, dup, zerolt, qbranch, abs_1, negate
abs_1:
	!word exit

negate_n:
	!byte 6
	!text "NEGATE"
	!word abs_n
negate:
	!word negate_c
negate_c:
	lda #0
	sec
	sbc _dtop
	sta _dtop
	lda #0
	sbc _dtop+1
	sta _dtop+1
	jmp next

oneplus_n:
	!byte 2
	!text "1+"
	!word negate_n
oneplus:
	!word oneplus_c
oneplus_c:
	inc _dtop
	bne oneplus_1
	inc _dtop+1
oneplus_1:
	jmp next

oneminus_n:
	!byte 2
	!text "1-"
	!word oneplus_n
oneminus:
	!word oneminus_c
oneminus_c:
	lda _dtop
	bne oneminus_1
	dec _dtop+1
oneminus_1:
	dec _dtop
	jmp next

twoplus_n:
	!byte 2 + NST_FLAG
	!text "2+"
	!word oneminus_n
twoplus:
	!word twoplus_c
twoplus_c:
	+ldax _dtop
	+incax 2
	+stax _dtop
	jmp next

twominus_n:
	!byte 2 + NST_FLAG
	!text "2-"
	!word twoplus_n
twominus:
	!word twominus_c
twominus_c:
	+ldax _dtop
	+decax 2
	+stax _dtop
	jmp next

twodiv_n:
	!byte 2
	!text "2/"
	!word twominus_n
twodiv:
	!word twodiv_c
twodiv_c:
	lda _dtop+1
	cmp #$80		; 6502 does not have native arithmetic shift right 
	ror _dtop+1
	ror _dtop
	jmp next

twomult_n:
	!byte 2
	!text "2*"
	!word twodiv_n
twomult:
	!word twomult_c
twomult_c:
	asl _dtop
	rol _dtop+1
	jmp next

;
; code lshift
; code rshift
;

lshift_n:
	!byte 6
	!text "LSHIFT"
	!word twomult_n
lshift:
	!word lshift_c
lshift_c:
	+dpop
	tax
	beq lshift_2
lshift_1:
	clc
	lda _dtop
	asl
	sta _dtop
	lda _dtop+1
	rol
	sta _dtop+1
	dex
	bne lshift_1
lshift_2:
	jmp next
			
rshift_n:
	!byte 6
	!text "RSHIFT"
	!word lshift_n
rshift:
	!word rshift_c
rshift_c:
	+dpop
	tax
	beq rshift_2
rshift_1:
	clc
	lda _dtop+1
	lsr
	sta _dtop+1
	lda _dtop
	ror
	sta _dtop
	dex
	bne rshift_1
rshift_2:
	jmp next

; Double-word math follows. Some words are in Core even if they work with double values. Depending on architecture, this
; part may be relatively easy to do or quite hard. RatC VM is clearly at the very hard end when it comes to division and
; multiplication. However, there are only two words that really need to be implemented there. There are different models
; but the easiest is to start from UM/MOD and UM*.

;
; : s>d dup 0< if -1 else 0 then ;
; : dnegate invert swap invert swap one m+ ;
; : dabs dup 0< if dnegate then ;
;

stod_n:
	!byte 3
	!text "S>D"
	!word rshift_n
stod:
	!word call, dup, zerolt, qbranch, stod_1, minusone, exit
stod_1:
	!word zero, exit

; Optional Double=number word set
dnegate_n:
	!byte 7
	!text "DNEGATE"
	!word stod_n
dnegate:
	!word call, invert, swap, invert, swap, one, mplus, exit 

; Optional Double-numbler word set
dabs_n:
	!byte 4
	!text "DABS"
	!word dnegate_n
dabs:
	!word call, dup, zerolt, qbranch, dabs_1, dnegate
dabs_1:
	!word exit

;
; : fm/mod dup >r sm/rem
;          over dup 0<> swap 0< r@ 0< xor and
;          if 1- swap r> + swap else rdrop then ;
;

fmmod_n:
	!byte 6
	!text "FM/MOD"
	!word dabs_n
fmmod:
	!word call, dup, tor, smrem, over, dup, zerone, swap, zerolt, rat, zerolt, xor, and_op, qbranch, fmmod_1, oneminus, swap, rfrom, add, swap, branch, fmmod_2
fmmod_1:
	!word rdrop
fmmod_2:
	!word exit

;
; : sm/rem 2dup xor >r ( Sign of the quotient) over >r ( Sign of the remainder)
;          abs >r dabs r> um/mod
;          swap r> 0< if negate then
;          swap r> 0< if negate then ;
;

smrem_n:
	!byte 6
	!text "SM/REM"
	!word fmmod_n
smrem:
	!word call, twodup, xor, tor, over, tor, abs, tor, dabs, rfrom, ummod
	!word swap, rfrom, zerolt, qbranch, smrem_1, negate
smrem_1:
	!word swap, rfrom, zerolt, qbranch, smrem_2, negate
smrem_2:
	!word exit

;
; code um/mod
;
; This is the "shift dividend left" algorithm, something like this
;
;      dividend to (_shigh, _slow)
;      repeat (bits per cell) times
;          (_shigh, _slow) <<= 1		(*)
;          if divisor <= _shigh			(*)
;              _shigh -= divisor
;              _slow++
;      _shigh to remainder
;      _slow to quotient
;
; (*) These two lines need to take in account the carry flag which essentially adds one extra bit
;

; As I suspected, the above algorithm is easier to implement on 6502 that it was on that RatVM "architecture". 6502 does
; not have hardware divide or multiply, so it has to be implemented this way.

_shigh		= _rscratch
_slow		= _wscratch
_sdiv		= _scratch

ummod_n:
	!byte 6
	!text "UM/MOD"
	!word smrem_n
ummod:
	!word ummod_c
ummod_c:
	+dpop
	+stax _sdiv
	+dpop
	+stax _shigh
	+ldax _dtop		; Note that we don't pull the last value from the stack!
	+stax _slow
	ldx #17
ummod_1:
	dex
	beq ummod_x
	asl _slow
	rol _slow+1
	rol _shigh
	rol _shigh+1
	bcs ummod_2		; If the carry is set, _shigh is considered larger than _sdiv due to extra high bit
	lda _sdiv+1
	cmp _shigh+1
	bcc ummod_2
	bne ummod_1
	lda _shigh
	cmp _sdiv
	bcc ummod_1
ummod_2:
	lda _shigh
	sec
	sbc _sdiv
	sta _shigh
	lda _shigh+1
	sbc _sdiv+1
	sta _shigh+1
	inc _slow
	bne ummod_1
	inc _slow+1
ummod_3:
	jmp ummod_1
ummod_x:
	+ldax _shigh
	+stax _dtop
	+ldax _slow
	+dpush
	jmp next

;
; : ud/mod >r 0 r@ um/mod rot rot r> um/mod rot ;
;

udmod_n:
	!byte 6 + NST_FLAG
	!text "UD/MOD"
	!word ummod_n
udmod:
	!word call, tor, zero, rat, ummod, rot, rot, rfrom, ummod, rot, exit

;
; code um*
;
; Another complex algorithm, using "shift product right" version
;
;    multiplicand to (_shigh, _slow) (_shigh is 0 as multiplicand is only 16 bits)
;    clear carry
;    repeat (bits per cell + 1) times
;        (_shigh, _slow) >>= 1						(pulling in carry)
;        if (carry is set)
;            _shigh += multiplier 					(saving carry)
;    (_shigh, _slow) to product
;
; Again, don't forget the carry on addition that would apply to the next shift

_smult		= _scratch

ummult_n:
	!byte 3
	!text "UM*"
	!word udmod_n
ummult:
	!word ummult_c
ummult_c:
	+dpop
	+stax _smult
	lda #0
	sta _shigh
	sta _shigh+1
	+ldax _dtop		; Note that we don't pull the last value from the stack!
	+stax _slow
	clc
	ldx #18
ummult_1:
	dex
	beq ummult_x
	ror _shigh+1	; Note that the carry flag is preserved over the loop adding one extra bit
	ror _shigh
	ror _slow+1
	ror _slow
	bcc ummult_1
	lda _shigh
	clc
	adc _smult
	sta _shigh
	lda _shigh+1
	adc _smult+1
	sta _shigh+1
	jmp ummult_1
ummult_x:
	+ldax _slow
	+stax _dtop
	+ldax _shigh
	+dpush
	jmp next

; UD* is not part of the standard but it is very convenient to use in formatting words
;
; : m* 2dup xor >r abs swap abs um* r> 0< if dnegate then ;
; : ud* dup >r um* drop swap r> um* rot + ; nonstandard
;

mmult_n:
	!byte 2
	!text "M*"
	!word ummult_n
mmult:
	!word call, twodup, xor, tor, abs, swap, abs, ummult, rfrom, zerolt, qbranch, mmult_1, dnegate
mmult_1:
	!word exit

udmult_n:
	!byte 3 + NST_FLAG
	!text "UD*"
	!word mmult_n
udmult:
	!word call, dup, tor, ummult, drop, swap, rfrom, ummult, rot, add, exit

;
; : m+ s>d d+ ;
;

; From the optional Double-number word set
mplus_n:
	!byte 2
	!text "M+"
	!word udmult_n
mplus:
	!word call, stod, dadd, exit

; ==============================================================================
; Logical operations. Note that all operations are performed bitwise

;
; code and
; code or
; code xor
; : invert -1 xor ;
;

and_n:
	!byte 3
	!text "AND"
	!word mplus_n
and_op:
	!word and_c
and_c:
	+dpop
	and _dtop
	sta _dtop
	txa
	and _dtop+1
	sta _dtop+1
	jmp next

or_n:
	!byte 2
	!text "OR"
	!word and_n
or:
	!word or_c
or_c:
	+dpop
	ora _dtop
	sta _dtop
	txa
	ora _dtop+1
	sta _dtop+1
	jmp next

xor_n:
	!byte 3
	!text "XOR"
	!word or_n
xor:
	!word xor_c
xor_c:
	+dpop
	eor _dtop
	sta _dtop
	txa
	eor _dtop+1
	sta _dtop+1
	jmp next

; Note that NOT has been removed from the standard.
invert_n:
	!byte 6
	!text "INVERT"
	!word xor_n
invert:
	!word call, minusone, xor, exit

; Find lowest zero (free) bit index
freebit_n:
	!byte 7 + NST_FLAG
	!text "FREEBIT"
	!word invert_n
freebit:
	!word freebit_x
freebit_x:
	lda _dtop
	sta _rscratch
	lda _dtop+1
	sta _rscratch+1
	lda #0
	tax
	sta _dtop
	sta _dtop+1
freebit_1:
	lda #1
	bit _rscratch
	beq freebit_2
	lsr _rscratch+1
	ror _rscratch
	inx
	jmp freebit_1
freebit_2:
	txa
	sta _dtop
	jmp next

setbit_n:
	!byte 6 + NST_FLAG
	!text "SETBIT"
	!word freebit_n
setbit:
	!word call, dup, peek, rot, one, swap, lshift, or, swap, poke, exit

clearbit_n:
	!byte 8 + NST_FLAG
	!text "CLEARBIT"
	!word setbit_n
clearbit:
	!word call, dup, peek, rot, one, swap, lshift, invert, and_op, swap, poke, exit

; ==============================================================================
; Comparisons

;
; code 0=
; code 0<
;

zeroeq_n:
	!byte 2
	!text "0="
	!word clearbit_n
zeroeq:
	!word zeroeq_c
zeroeq_c:
	ldx #255
	lda _dtop
	ora _dtop+1
	beq zeroeq_1
	ldx #0
zeroeq_1:
	stx _dtop
	stx _dtop+1
	jmp next

zerolt_n:
	!byte 2
	!text "0<"
	!word zeroeq_n
zerolt:
	!word zerolt_c
zerolt_c:
	ldx #255
	lda _dtop+1
	bmi zerolt_1
	ldx #0
zerolt_1:
	stx _dtop
	stx _dtop+1
	jmp next

;
;	: 0> 0 swap < ;
;	: 0<> 0= 0= ;
;	: = - 0= ;
;	: <> - 0<> ;
;

zerogt_n:
	!byte 2
	!text "0>"
	!word zerolt_n
zerogt:
	!word call, zero, swap, less, exit
			
zerone_n:
	!byte 3
	!text "0<>"
	!word zerogt_n
zerone:
	!word call, zeroeq, zeroeq, exit

equal_n:
	!byte 1
	!text "="
	!word zerone_n
equal:
	!word call, sub, zeroeq, exit

notequal_n:
	!byte 2
	!text "<>"
	!word equal_n
notequal:
	!word call, sub, zerone, exit


;
; code <
;
; Careful here. Some implementations have it as ": < - 0< ;" and it works... sometimes.
; Signed comparison on 6502 is surprisingly non-trivial. Refer to http://www.6502.org/tutorials/compare_beyond.html
; for details and tutorial
less_n:
	!byte 1
	!text "<"
	!word notequal_n
less:
	!word less_c
less_c:
	+dpop
	+stax _scratch
	sec
	lda _dtop+1
	sbc _scratch+1
	bvc less_y1
	eor #$80
less_y1:
	bmi less_y4
	bvc less_y2
	eor #$80
less_y2:
	bne less_y3
	lda _dtop
	sbc _scratch
	bcc less_y4
less_y3:
	lda #0
	jmp less_y5
less_y4:
	lda #255
less_y5:
	sta _dtop
	sta _dtop+1
	jmp next

;
;	: > swap < ;
;	: max 2dup < if swap then drop ;
;	: min 2dup > if swap then drop ;
;

greater_n:
	!byte 1
	!text ">"
	!word less_n
greater:
	!word call, swap, less, exit

max_n:
	!byte 3
	!text "MAX"
	!word greater_n
max:
	!word call, twodup, less, qbranch, max_1, swap
max_1:
	!word drop, exit

min_n:
	!byte 3
	!text "MIN"
	!word max_n
min:
	!word call, twodup, greater, qbranch, min_1, swap
min_1:
	!word drop, exit

;
;	code u<
;
uless_n:
	!byte 2
	!text "U<"
	!word min_n
uless:
	!word uless_c
uless_c:
	+dpop
	+stax _scratch
	lda _dtop+1
	cmp _scratch+1
	bcc uless_t
	bne uless_f
	lda _dtop
	cmp _scratch
	bcc uless_t
uless_f:
	lda #0
	jmp uless_1
uless_t:
	lda #255
uless_1:
	sta _dtop
	sta _dtop+1
	jmp next

;
;	: u> swap u< ;
;

ugreater_n:
	!byte 2
	!text "U>"
	!word uless_n
ugreater:
	!word call, swap, uless, exit

;
;	-1 constant true
;	0 constant false
;

true_n:
	!byte 4
	!text "TRUE"
	!word ugreater_n
true:
	!word doconst, VAL_TRUE

false_n:
	!byte 5
	!text "FALSE"
	!word true_n
false:
	!word doconst, VAL_FALSE

;
;	: within over - >r - r> u< ;
;

within_n:
	!byte 6
	!text "WITHIN"
	!word false_n
within:
	!word call, over, sub, tor, sub, rfrom, uless, exit

; ==============================================================================
; Base stack operations.

;
;	code dup
;	code drop
;	code over
;	code swap
;

dup_n:
	!byte 3
	!text "DUP"
	!word within_n
dup:
	!word dup_c
dup_c:
	+ldax _dtop
	+dpush
	jmp next

drop_n:
	!byte 4
	!text "DROP"
	!word dup_n
drop:
	!word drop_c
drop_c:
	+dpop
	jmp next

over_n:
	!byte 4
	!text "OVER"
	!word drop_n
over:
	!word over_c
over_c:
	ldy #3
	lda (_dstack),y
	tax
	dey
	lda (_dstack),y
	+dpush
	jmp next

swap_n:
	!byte 4
	!text "SWAP"
	!word over_n
swap:
	!word swap_c
swap_c:
	+dpop
	ldy _dtop
	sta _dtop
	tya
	pha
	ldy _dtop+1
	stx _dtop+1
	tya
	tax
	pla
	+dpush
	jmp next

;
;	: nip swap drop ;
;	: tuck swap over ;
;

nip_n:
	!byte 3
	!text "NIP"
	!word swap_n
nip:
	!word call, swap, drop, exit

tuck_n:
	!byte 4
	!text "TUCK"
	!word nip_n
tuck:
	!word call, swap, over, exit

;
; : rot >r swap r> swap ;
;

rot_n:
	!byte 3
	!text "ROT"
	!word tuck_n
rot:
	!word call, tor, swap, rfrom, swap, exit

;
;	code pick
;	code roll ; using reference implementation from forth-standard.org instead
;
; TODO - maybe check stack in both calls

pick_n:
	!byte 4
	!text "PICK"
	!word rot_n
pick:
	!word pick_c
pick_c:
	asl _dtop
	rol _dtop+1
	lda _dstack
	clc
	adc _dtop
	sta _rscratch
	lda _dstack+1
	adc _dtop+1
	sta _rscratch+1
	ldy #2
	lda (_rscratch),y
	sta _dtop
	iny
	lda (_rscratch),y
	sta _dtop+1
	jmp next

roll_n:
	!byte 4
	!text "ROLL"
	!word pick_n
roll:
	!word call, dup, qbranch, roll_1 ; Using reference implementation
	!word swap, tor, oneminus, roll, rfrom, swap, exit
roll_1:
	!word drop, exit

;
;	code depth
;

depth_n:
	!byte 5
	!text "DEPTH"
	!word roll_n
depth:
	!word depth_c
depth_c:
	lda #<DSTACK_INIT
	sec
	sbc _dstack
	tay
	lda #>DSTACK_INIT
	sbc _dstack+1
	lsr
	tax
	tya
	ror
	+dpush
	jmp next

;
;	: 2drop drop drop ;
;	: 2dup over over ;
;	: 2swap >r rot rot >r rot rot ;
;	: 2over >r >r 2dup r> r> 2swap ;
;

twodrop_n:
	!byte 5
	!text "2DROP"
	!word depth_n
twodrop:
	!word call, drop, drop, exit

twodup_n:
	!byte 4
	!text "2DUP"
	!word twodrop_n
twodup:
	!word call, over, over, exit

twoswap_n:
	!byte 5
	!text "2SWAP"
	!word twodup_n
twoswap:
	!word call, tor, rot, rot, rfrom, rot, rot, exit

twoover_n:
	!byte 5
	!text "2OVER"
	!word twoswap_n
twoover:
	!word call, tor, tor, twodup, rfrom, rfrom, twoswap, exit

;
;	: ?dup dup if dup then ;
;

qdup_n:
	!byte 4
	!text "?DUP"
	!word twoover_n
qdup:
	!word call, dup, qbranch, qdup_1, dup 
qdup_1:
	!word exit

; ==============================================================================
; Standard cell/char size words and alignment (which do nothing on this architecture)

;
; : cell+ 2+ ;
; : cells dup + ;
; : char+ 1+ ;
; : chars ;
; : align ;
; : aligned ;
;

cellplus_n:
	!byte 5
	!text "CELL+"
	!word qdup_n
cellplus:
	!word call, twoplus, exit
			
cells_n:
	!byte 5
	!text "CELLS"
	!word cellplus_n
cells:
	!word call, dup, add, exit
			
charplus_n:
	!byte 5
	!text "CHAR+"
	!word cells_n
charplus:
	!word call, oneplus, exit
			
chars_n:
	!byte 5
	!text "CHARS"
	!word charplus_n
chars:
	!word call, exit	; that's correct, just do nothing

align_n:
	!byte 5
	!text "ALIGN"
	!word chars_n
align:
	!word call, exit

aligned_n:
	!byte 7
	!text "ALIGNED"
	!word align_n
aligned:
	!word call, exit

; ==============================================================================
; Words working with the return stack. These are probably among the most dangerous words in the language,
; any abuse would likely result in the system crash. An important aspect that all of these have to
; be implemented natively (don't try to implement RDROP as R> DROP - it won't work)

;
; code r>
; code >r
; code r@
; code rdrop nonstandard
; code 2>r
; code 2r>
; code 2r@
;

rfrom_n:
	!byte 2
	!text "R>"
	!word aligned_n
rfrom:
	!word rfrom_c
rfrom_c:
	+rpop
	+dpush
	jmp next

tor_n:
	!byte 2
	!text ">R"
	!word rfrom_n
tor:
	!word tor_c
tor_c:
	+dpop
	+rpush
	jmp next

rat_n:
	!byte 2
	!text "R@"
	!word tor_n
rat:
	!word rat_c
rat_c:
	ldy #3
	lda (_rstack),y
	tax
	dey
	lda (_rstack),y
	+dpush
	jmp next

rdrop_n:
	!byte 5 + NST_FLAG
	!text "RDROP"
	!word rat_n
rdrop:
	!word rdrop_c
rdrop_c:
	+rpop
	jmp next

twotor_n:
	!byte 3
	!text "2>R"
	!word rdrop_n
twotor:
	!word twotor_c
twotor_c:
	+dpop
	+stax _rscratch
	+dpop
	+rpush
	+ldax _rscratch
	+rpush
	jmp next

tworfrom_n:
	!byte 3
	!text "2R>"
	!word twotor_n
tworfrom:
	!word tworfrom_c
tworfrom_c:
	+rpop
	+stax _rscratch
	+rpop
	+dpush
	+ldax _rscratch
	+dpush
	jmp next

tworat_n:
	!byte 3
	!text "2R@"
	!word tworfrom_n
tworat:
	!word tworat_c
tworat_c:
	ldy #5: 
	lda (_rstack),y
	tax
	dey
	lda (_rstack),y
	+dpush
	ldy #3
	lda (_rstack),y
	tax
	dey
	lda (_rstack),y
	+dpush
	jmp next

; ==============================================================================
; Basic memory operations

;
; code @
; code c@
; code !
; code c!
; : 2! swap over ! cell+ ! ;
; : 2@ dup cell+ @ swap peek ;
;

peek_n:
	!byte 1
	!text "@"
	!word tworat_n
peek:
	!word peek_c
peek_c:
	ldy #1
	lda (_dtop),y
	tax
	dey
	lda (_dtop),y
	+stax _dtop
	jmp next

cpeek_n:
	!byte 2
	!text "C@"
	!word peek_n
cpeek:
	!word cpeek_c
cpeek_c:
	ldy #0
	lda (_dtop),y
	sta _dtop
	sty _dtop+1
	jmp next

poke_n:
	!byte 1
	!text "!"
	!word cpeek_n
poke:
	!word poke_c
poke_c:
	+dpop
	+stax _wscratch
	+dpop
	ldy #0
	sta (_wscratch),y
	txa
	iny
	sta (_wscratch),y
	jmp next

cpoke_n:
	!byte 2
	!text "C!"
	!word poke_n
cpoke:
	!word cpoke_c
cpoke_c:
	+dpop
	+stax _wscratch
	+dpop
	ldy #0
	sta (_wscratch),y
	jmp next

twopoke_n:
	!byte 2
	!text "2!"
	!word cpoke_n
twopoke:
	!word call, swap, over, poke, cellplus, poke, exit

twopeek_n:
	!byte 2
	!text "2@"
	!word twopoke_n
twopeek:
	!word call, dup, cellplus, peek, swap, peek, exit

; ==============================================================================
; Literal. One of the most common words to see in the compiled code - will take the next parameter and
; push it to stack

;
; code lit
; Alternative but slower: : lit r@ peek r> cell+ >r ; nonstandard
;

lit_n:
	!byte 3 + NST_FLAG
	!text "LIT"
	!word twopeek_n
lit:
	!word lit_c
lit_c:
	ldy #1
	lda (_ri),y
	tax
	dey
	lda (_ri),y
	+dpush
	+ldax _ri
	+incax 2
	+stax _ri
	jmp next

; ==============================================================================
; Numeric output. Forth approach is a bit odd but extremely powerful

;
; variable base
; : <@ _hldend _hld ! ;
; : # base @ ud/mod rot '0' + dup '9' > if 7 + then hold ; 
; : #s begin # 2dup or 0= until ; 
; : #> 2drop _hld @ _hldend over - ;
; : hold _hld @ 1 - dup _hld ! c! ;
; : holds begin dup while 1- 2dup + c@ hold again 2drop ;
; : sign 0< if '-' hold then ;
;

base_n:
	!byte 4
	!text "BASE"
	!word lit_n
base:
	!word doconst, _base

bhash_n:
	!byte 2
	!text "<#"
	!word base_n
bhash:
	!word call, lit, _hldend, lit, _hld, poke, exit

hash_n:
	!byte 1
	!text "#"
	!word bhash_n
hash:
	!word call, base, peek, udmod, rot, lit, '0', add
	!word dup, lit, '9', greater, qbranch, hash_1, lit, 7, add
hash_1:
	!word hold, exit

hashs_n:
	!byte 2
	!text "#S"
	!word hash_n
hashs:
	!word call
hashs_1:
	!word hash, twodup, or, zeroeq, qbranch, hashs_1, exit

hashb_n:
	!byte 2
	!text "#>"
	!word hashs_n
hashb:
	!word call, twodrop, lit, _hld, peek, lit, _hldend, over, sub, exit

hold_n:
	!byte 4
	!text "HOLD"
	!word hashb_n
hold:
	!word call, lit, _hld, peek, one, sub, dup, lit, _hld, poke, cpoke, exit

holds_n:
	!byte 5
	!text "HOLDS"
	!word hold_n
holds:
	!word call
holds_1:
	!word dup, qbranch, holds_2, oneminus, twodup, add, cpeek, hold, branch, holds_1
holds_2:
	!word twodrop, exit

sign_n:
	!byte 4
	!text "SIGN"
	!word holds_n
sign:
	!word call, zerolt, qbranch, sign_1, lit, '-', hold
sign_1:
	!word exit

;
; : d.r >r dup >r dabs <# #s r> sign #> r> over - spaces type ;
; : d. 0 d.r space ;
; : .r swap s>d rot d.r ;
; : u. 0 d.
; : u.r 0 swap d.r ;
; : . s>d d. ;
;

ddotr_n:
	!byte 3
	!text "D.R"
	!word sign_n
ddotr:
	!word call, tor, dup, tor, dabs, bhash, hashs, rfrom, sign, hashb, rfrom, over, sub, spaces, type, exit

ddot_n:
	!byte 2
	!text "D."
	!word ddotr_n
ddot:
	!word call, zero, ddotr, space, exit

dotr_n:
	!byte 2
	!text ".R"
	!word ddot_n
dotr:
	!word call, swap, stod, rot, ddotr, exit

udot_n:
	!byte 2
	!text "U."
	!word dotr_n
udot:
	!word call, zero, ddot, exit

udotr_n:
	!byte 3
	!text "U.R"
	!word udot_n
udotr:
	!word call, zero, swap, ddotr, exit

dot_n:
	!byte 1
	!text "."
	!word udotr_n
dot:
	!word call, stod, ddot, exit

;
; : decimal 10 base ! ;
; : hex 16 base ! ;
;

decimal_n:
	!byte 7
	!text "DECIMAL"
	!word dot_n
decimal:
	!word call, lit, 10, base, poke, exit

hex_n:
	!byte 3
	!text "HEX"
	!word decimal_n
hex:
	!word call, lit, 16, base, poke, exit

; ==============================================================================
; HERE, comma, C,, etc.

;
; : +! dup @ rot + swap ! ;
; : here _here @ ;
; : allot _here +!
; : , here 2 allot ! ;
; : c, here 1 allot c! ;
;

incpoke_n:
	!byte 2
	!text "+!"
	!word hex_n
incpoke:
	!word call, dup, peek, rot, add, swap, poke, exit

here_n:
	!byte 4
	!text "HERE"
	!word incpoke_n
here:
	!word call, lit, _here, peek, exit

allot_n:
	!byte 5
	!text "ALLOT"
	!word here_n
allot:
	!word call, lit, _here, incpoke, exit

comma_n:
	!byte 1
	!text ","
	!word allot_n
comma:
	!word call, here, two, allot, poke, exit

ccomma_n:
	!byte 2
	!text "C,"
	!word comma_n
ccomma:
	!word call, here, one, allot, cpoke, exit

; ==============================================================================
; Branching. All control strucutres are built on these two. The words are not in the standard
; but commonly used. ?BRANCH is sometimes named 0BRANCH
; There is an exotic way to implement BRANCH as
; : BRANCH R> @ >R ;
; This approach does not work for ?BRANCH due to chicken-and-egg problem

;
; code branch nonstandard
; code ?branch nonstandard
;

branch_n:
	!byte 6 + NST_FLAG
	!text "BRANCH"
	!word ccomma_n
branch:
	!word branch_c
branch_c:
	ldy #1
	lda (_ri),y
	tax
	dey
	lda (_ri),y
	+stax _ri
	jmp next

qbranch_n:
	!byte 7 + NST_FLAG
	!text "?BRANCH"
	!word branch_n
qbranch:
	!word qbranch_c
qbranch_c:
	+dpop
	stx _rscratch
	ora _rscratch
	beq branch_c
	+ldax _ri
	+incax 2
	+stax _ri
	jmp next

; ==============================================================================
; Line input support

tib_n:
	!byte 3 + NST_FLAG
	!text "TIB"
	!word qbranch_n
tib:
	!word call, lit, _source, peek, twoplus, twoplus, twoplus, peek, exit

ptrin_n:
	!byte 3
	!text ">IN"
	!word tib_n
ptrin:
	!word call, lit, _source, peek, twoplus, twoplus, exit

numtib_n:
	!byte 4 + NST_FLAG
	!text "#TIB"
	!word ptrin_n
numtib:
	!word call, lit, _source, peek, twoplus, twoplus, twoplus, twoplus, exit

source_n:
	!byte 6
	!text "SOURCE"
	!word numtib_n
source:
	!word call, tib, numtib, peek, exit

sourceid_n:
	!byte 9
	!text "SOURCE-ID"
	!word source_n
sourceid:
	!word call, lit, _source, peek, twoplus, peek, exit

accept_n:
	!byte 6
	!text "ACCEPT"
	!word sourceid_n
accept:
	!word accept_c
accept_c:
	+dpop
	sta _rscratch ; Note that this only works properly for small numbers, but this is platform-consistent anyway
	ldy #0
accept_1:
	jsr CHRIN
	cmp #NEW_LINE
	beq accept_2
	sta (_dtop),y
	iny
	bne accept_1
	tya
	cmp _rscratch
	bmi accept_1
accept_2:
	sty _dtop
	lda #0
	sta _dtop+1
	jmp next

key_n:
	!byte 3
	!text "KEY"
	!word accept_n
key:
	!word key_c
key_c:
	jsr CHRIN			; TODO - this will echo the character, which contradicts the standard
	ldx #0
	+dpush
	jmp next

; : refill source-id 0< if false exit then
;          source-id 0= if cr ." Ok" cr tib 80 accept #tib ! 0 >in ! true exit then
;          source-id file-position drop _source 10 + 2!
;          tib 98 source-id read-line 0= and
;          if #tib ! 0 >in ! true exit then
;          drop false ;

; Note that slightly longer lines are being read from file
refill_n:
	!byte 6
	!text "REFILL"
	!word key_n
refill:
	!word call, sourceid, zerolt, qbranch, refill_1, false, exit	; "EVALUATE" - no refill
refill_1:
	!word sourceid, zeroeq, qbranch, refill_2, cr, lit, prompt, count, type, cr, tib, lit, 80, accept, numtib, poke, zero, ptrin, poke, true, exit	; console
refill_2:
	!word sourceid, fileposition, drop, lit, _source, peek, lit, 10, add, twopoke
	!word tib, lit, 98, sourceid, readline, zeroeq, and_op, qbranch, refill_3
	!word numtib, poke, zero, ptrin, poke, true, exit	 ; file (note that the position is saved _before_ the refill)
refill_3:
	!word drop, false, exit
prompt:
	!byte 2
	!text "OK"

;
; : save-input _source @ dup >r @ begin dup while dup 2* r@ + @ swap 1- again drop r> @ ;
;
; : restore-input over source-id = if
;                 source-id 0> if 6 pick 6 pick source-id reposition-file refill 2drop then
;                 begin dup while dup roll over 2* _source @ + ! 1- again drop false
;                 else true then ;
;

saveinput_n:
	!byte 10
	!text "SAVE-INPUT"
	!word refill_n
saveinput:
	!word call, lit, _source, peek, dup, tor, peek
saveinput_1:
	!word dup, qbranch, saveinput_2, dup, twomult, rat, add, peek, swap, oneminus, branch, saveinput_1
saveinput_2:
	!word drop, rfrom, peek, exit

restoreinput_n:
	!byte 13
	!text "RESTORE-INPUT"
	!word saveinput_n
restoreinput:
	!word call, over, sourceid, equal, qbranch, restoreinput_3
	!word sourceid, zerogt, qbranch, restoreinput_1
	!word lit, 6, pick, lit, 6, pick, sourceid, repositionfile, refill, twodrop
restoreinput_1:
	!word dup, qbranch, restoreinput_2, dup, roll, over, twomult, lit, _source, peek, add, poke, oneminus, branch, restoreinput_1
restoreinput_2:
	!word drop, false, exit
restoreinput_3:
	!word true, exit

; ==============================================================================
; Some basic text output

emit_n:
	!byte 4
	!text "EMIT"
	!word restoreinput_n
emit:
	!word emit_c
emit_c:
	+dpop
	jsr CHROUT
	jmp next

cr_n:
	!byte 2
	!text "CR"
	!word emit_n
cr:
	!word call, lit, NEW_LINE, emit, exit

bl_n:
	!byte 2
	!text "BL"
	!word cr_n
bl:
	!word doconst, ' '

space_n:
	!byte 5
	!text "SPACE"
	!word bl_n
space:
	!word call, bl, emit, exit

type_n:
	!byte 4
	!text "TYPE"
	!word space_n
type:
	!word call
type_1:
	!word dup, zerone, qbranch, type_done
	!word oneminus, swap, dup, cpeek, emit, oneplus, swap, branch, type_1
type_done:
	!word drop, drop, exit

count_n:
	!byte 5
	!text "COUNT"
	!word type_n
count:
	!word call, dup, oneplus, swap, cpeek, exit

; ==============================================================================
; Word lookup. This is where the complex things begin.

word_n:
	!byte 4
	!text "WORD"
	!word count_n
word:
	!word call, tor, source, swap, ptrin, peek, add
word_1:
	!word over, ptrin, peek, greater, qbranch, word_2
	!word dup, cpeek, lit, 127, and_op, rat, equal, qbranch, word_2 ; Note &127, this is a workaround for peculiar C64 annoyance
	!word ptrin, peek, oneplus, ptrin, poke, oneplus, branch, word_1
word_2:
	!word twodrop, rfrom, parse, dup, lit, _wordbuf, cpoke
	!word lit, _wordbuf, oneplus, swap, cmove, lit, _wordbuf, exit

parse_n:
	!byte 5
	!text "PARSE"
	!word word_n
parse:
	!word call, tor, source, ptrin, peek, sub, oneplus, tor, ptrin, peek, add, dup, zero
parse_1:
	!word over, cpeek, rfrom, oneminus, dup, qbranch, parse_3
	!word swap, lit, 127, and_op, rat, equal, qbranch, parse_2 ; SAME AS ABOVE
	!word drop, swap, drop, rdrop, ptrin, dup, peek, oneplus, swap, poke, exit
parse_2:
	!word tor, swap, oneplus, swap, oneplus, ptrin, dup, peek, oneplus, swap, poke, branch, parse_1
parse_3:
	!word twodrop, swap, drop, rdrop, exit

parsename_n:
	!byte 10
	!text "PARSE-NAME"
	!word parse_n
parsename:
	!word call, source, swap, ptrin, peek, add
parsename_1:
	!word over, ptrin, peek, greater, qbranch, parsename_2
	!word dup, cpeek, bl, equal, qbranch, parsename_2
	!word ptrin, peek, oneplus, ptrin, poke, oneplus, branch, parsename_1
parsename_2:
	!word twodrop, bl, parse, exit

; Forth systems typically have a few words to move between different parts of a vocabulary word. In the indirect
; threaded code the only non-trivial move is the one between LFA and NFA. In this particular model it abuses the
; fact that the maximum NFA length is 32+1 and the name cannot include characters with codes below 32. 
lfatonfa_n:
	!byte 6 + NST_FLAG
	!text "L>NAME"
	!word parsename_n
lfatonfa:
	!word call, oneminus, zero
lfatonfa_1:
	!word over, cpeek, lit, NAMEMASK, and_op, over, notequal, qbranch, lfatonfa_2
	!word swap, oneminus, swap, oneplus, dup, lit, 32, equal, qbranch, lfatonfa_1
	!word drop, drop, zero, exit
lfatonfa_2:
	!word drop, exit

tobody_n:
	!byte 5
	!text ">BODY"
	!word lfatonfa_n
tobody:
	!word call, twoplus, exit

context_n:
	!byte 7 + NST_FLAG
	!text "CONTEXT"
	!word tobody_n
context:
	!word doconst, _context

latest_n:
	!byte 6 + NST_FLAG
	!text "LATEST"
	!word context_n
latest:
	!word call, lit, _latest, peek, exit


cstr1 = _dtop
clen1 = _scratch
cstr2 = _rscratch
clen2 = _wscratch

; COMPARE became standard in the later versions of the language.
; In optional String word set
; This one still can be optimized further
compare_n:
	!byte 7
	!text "COMPARE" ; (caddr1, u1, caddr2, u2 -> n)
	!word latest_n
compare:
	!word compare_x
compare_x:
	+dpop
	+stax clen2
	+dpop
	+stax cstr2
	+dpop
	+stax clen1
	; and cstr1 is already where it should be. No need to pop as the result will be written there

	ldy #0
	
compare_loop:
	lda clen1
	ora clen1+1
	bne compare_check2
	ora clen2
	ora clen2+1
	bne compare_gt	; clen1 < clen2
;	lda #$0
	sta _dtop
	beq compare_res	; reached the end of both strings and all characters match
compare_check2:
	lda clen2
	ora clen2+1
	beq compare_lt	; clen1 > clen2

	lda (cstr1),y
	cmp (cstr2),y
	bcc compare_gt
	beq compare_next
compare_lt:
	lda #$FF
	sta _dtop
compare_res:
	sta _dtop+1
	jmp next
compare_gt:
	lda #1
	sta _dtop
	lda #0
	beq compare_res
	
compare_next:
	iny
	bne compare_next1:
	inc cstr1+1
	inc cstr2+1

compare_next1:
	lda clen1
	bne compare_next2
	dec clen1+1
compare_next2:
	dec clen1
	
	lda clen2
	bne compare_next3
	dec clen2+1
compare_next3:
	dec clen2
	jmp compare_loop


strict_n:
	!byte 6 + NST_FLAG
	!text "STRICT"
	!word compare_n
strict:
	!word call, true, lit, _strict, poke, exit

; The only non-standard word that is visible in "strict" mode
extended_n:
	!byte 8
	!text "EXTENDED"
	!word strict_n
extended:
	!word call, false, lit, _strict, poke, exit

;
; : (find) (;code) nonstandard
;
; : find dup >r count (find) dup
;        if rdrop dup count #namemask and + 2+
;           swap c@ #immflag and -1 swap if negate then
;        else >r swap then ;
;

; Performance of FIND is critical for the interpretation speed, so it is worth to rewrite it in assembly. (FIND) is scanning through
; the vocabulary, so it has much higher impact

xfind_n:
	!byte 6 + NST_FLAG
	!text "(FIND)"		; caddr, n -> NFA | 0
	!word extended_n
xfind:
	!word xfind_x
xfind_x:
	+dpop
	sta _scratch
	+ldax _context
	+stax _rscratch
	
xfind_compare:
	ldy #0				; compare the word length at the scan pointer to _scratch
	lda (_rscratch),y
	and #NAMEMASK
	tax
	cmp _scratch
	bne xfind_nextword	; nope, next. note that the length is already in X
	
	tay					; the result of the above is that A has the length which is also a proper offset to the last char in scanned word
xfind_cmpchar:
	lda (_rscratch),y
	dey					; and one past the last char in test word
	bmi xfind_found		; the last character has been compared already? good, that's the result
	cmp (_dtop),y
	beq xfind_cmpchar	; same char, continue. otherwise, next word

xfind_nextword:
	txa					; expect current length in X
	tay
	iny					; name + name byte, now pointing at the LFA
	lda (_rscratch),y
	tax
	iny
	lda (_rscratch),y
	beq xfind_nomorewords	; there is no way any words reside in the zero page
	sta _rscratch+1
	txa
	sta _rscratch
	jmp xfind_compare

xfind_nomorewords:
	sta _dtop
	sta _dtop+1
	jmp next

xfind_found:
	+ldax _rscratch
	+stax _dtop
	jmp next


find_n:
	!byte 4
	!text "FIND"
	!word xfind_n
find:
	!word call, dup, tor, count, xfind, dup, qbranch, find_1
	!word rdrop, dup, count, lit, NAMEMASK, and_op, add, twoplus
	!word swap, cpeek, lit, IMM_FLAG, and_op, minusone, swap, qbranch, find_2, negate, exit
find_1:
	!word rfrom, swap
find_2:
	!word exit

immediate_n:
	!byte 9
	!text "IMMEDIATE"
	!word find_n
immediate:
	!word call, latest, dup, cpeek, lit, IMM_FLAG, or, swap, cpoke, exit

nonstandard_n:
	!byte 11 + NST_FLAG
	!text "NONSTANDARD"
	!word immediate_n
nonstandard:
	!word call, latest, dup, cpeek, lit, IMM_FLAG, or, swap, cpoke, exit

xdigit_n:
	!byte 7 + NST_FLAG
	!text "(DIGIT)"
	!word nonstandard_n
xdigit:
	!word call, dup, lit, 48, less, qbranch, xdigit_1
xdigit_x:
	!word drop, minusone, exit
xdigit_1:
	!word dup, lit, 58, less, qbranch, xdigit_2, lit, 48, sub, exit ; '0'-'9'
xdigit_2:
	!word dup, lit, 65, less, qbranch, xdigit_3, branch, xdigit_x
xdigit_3:
	!word dup, lit, 91, less, qbranch, xdigit_4, lit, 55, sub, exit ; 'A'-'Z'
xdigit_4:
	!word dup, lit, 97, less, qbranch, xdigit_5, branch, xdigit_x
xdigit_5:
	!word dup, lit, 123, less, qbranch, xdigit_x, lit, 87, sub, exit ; 'a'-'z'

tonumber_n:
	!byte 7
	!text ">NUMBER"
	!word xdigit_n
tonumber:
	!word call
tonumber_1:
	!word dup, zerogt, qbranch, tonumber_3									; no more digits left?
	!word over, cpeek, xdigit, dup, zerolt, zeroeq, qbranch, tonumber_2	; not a possible digit?
	!word dup, base, peek, less, qbranch, tonumber_2						; not a digit in current base?
	!word swap, oneminus, tor, swap, oneplus, tor, tor
	!word base, peek, udmult, rfrom, mplus, rfrom, rfrom
	!word branch, tonumber_1												; and repeat
tonumber_2:
	!word drop
tonumber_3:
	!word exit

number_n:
	!byte 6 + NST_FLAG
	!text "NUMBER"
	!word tonumber_n
number:
	!word call, count, base, peek, tor
	!word dup, lit, 3, equal, two, pick, cpeek, lit, 39, equal, and_op			; character as 'c'
	!word two, pick, two, add, cpeek, lit, 39, equal, and_op, qbranch, number_8
	!word drop, oneplus, cpeek, branch, number_5
number_8:
	!word dup, one, greater, qbranch, number_9
	!word over, cpeek, lit, 35, equal, qbranch, number_11, decimal, branch, number_10
number_11:
	!word over, cpeek, lit, 36, equal, qbranch, number_12, hex, branch, number_10
number_12:
	!word over, cpeek, lit, 37, equal, qbranch, number_9, two, base, poke
number_10:
	!word swap, oneplus, swap, oneminus
number_9:
	!word twodup, false, tor, over, cpeek, lit, 45, equal, qbranch, number_1
	!word rdrop, true, tor, oneminus, swap, oneplus, swap
number_1:
	!word zero, dup, twoswap, tonumber, dup, qbranch, number_4
	!word one, equal, swap, cpeek, lit, 46, equal, and_op, qbranch, number_7	; one unconverted char and it's '.'?
	!word rfrom, qbranch, number_2, dnegate
number_2:
	!word twoswap, twodrop, state, peek, qbranch, number_3, compile, lit, swap, comma, compile, lit, comma
number_3:
	!word branch, number_6
number_4:
	!word twodrop, twoswap, twodrop, drop, rfrom, qbranch, number_5, negate
number_5:
	!word state, peek, qbranch, number_6, compile, lit, comma
number_6:
	!word rfrom, base, poke, exit
number_7:
	!word twodrop, type, xabortq
	!byte 2
	!text " ?"


; ==============================================================================
; Nice service word that prints the entire list of supported words.

; In optional Programming-Tools word set
words_n:
	!byte 5
	!text "WORDS"
	!word number_n
words:
	!word call, zero, context, peek
words_1:
	!word dup, count, dup, lit, NST_FLAG, and_op, lit, _strict, peek, and_op, swap
	!word lit, NAMEMASK, and_op, dup, tor, swap, zeroeq, qbranch, words_2
	!word dup, zerone, qbranch, words_2
	!word type, bl, emit, swap, oneplus, swap, branch, words_3
words_2:
	!word twodrop
words_3:
	!word rfrom, oneplus, add, peek, dup, zeroeq, qbranch, words_1
	!word drop, cr, dot, lit, words_n, count, type, exit

; ==============================================================================
; Outer interpreter

state_n:
	!byte 5
	!text "STATE"
	!word words_n
state:
	!word doconst, _state

qcomp_n:
	!byte  5 + NST_FLAG
	!text "?COMP"
	!word state_n
qcomp:
	!word call, state, peek, zeroeq, qbranch, qcomp_2
	!word rat, twominus, twominus, dup, peek, lit, call, equal, qbranch, qcomp_1	; if ?COMP immediately follows CALL
	!word twominus, lfatonfa, count, lit, NAMEMASK, and_op, type, space					; output the word name with error
qcomp_1:
	!word xabortq
	!byte 20
	!text "REQUIRES COMPILATION"
qcomp_2:
	!word exit

qstack_n:
	!byte 6 + NST_FLAG
	!text "?STACK"
	!word qcomp_n
qstack:
	!word call, depth, dup, zerolt, swap, lit, STACKLIMIT, greater, or, qbranch, qstack_1
	!word xabortq
	!byte 11
	!text "STACK FAULT"
qstack_1:
	!word exit

interpret_n:
	!byte 9 + NST_FLAG
	!text "INTERPRET"
	!word qstack_n
interpret:
	!word call
interpret_1:
	!word qstack, bl, word, dup, cpeek, qbranch, interpret_done	; get the next word if any
	!word state, peek, qbranch, interpret_int
	!word find, dup, qbranch, comp_num
	!word zerolt, qbranch, comp_imm		; compiling now
	!word comma, branch, interpret_1		; regular word in compile mode
comp_imm:
	!word execute, branch, interpret_1		; immediate word in compile mode
comp_num:
	!word drop, number, branch, interpret_1
interpret_int:
	!word find, qbranch, int_num			; interpreting now
	!word execute, branch, interpret_1		; any word in interpreter mode
int_num:
	!word number, branch, interpret_1
interpret_done:
	!word drop, refill, zeroeq, qbranch, interpret_1
	!word closesource, exit

closesource_n:
	!byte 12 + NST_FLAG
	!text "CLOSE-SOURCE"
	!word interpret_n
closesource:
	!word call, sourceid, zerone, qbranch, closesource_2						; nothing to do with console source
	!word sourceid, zerogt, qbranch, closesource_1
	!word sourceid, closefile, drop, minusone, lit, _ibufcount, incpoke		; close file and release the buffer
closesource_1:
	!word lit, _source, dup, peek, dup, peek, oneplus, cells, add, swap, poke	; this will close the last frame
closesource_2:
	!word exit

evaluate_n:
	!byte 8
	!text "EVALUATE"
	!word closesource_n
evaluate:
	!word call, lit, _source, peek
	!word twominus, swap, over, poke
	!word twominus, swap, over, poke
	!word twominus, zero, over, poke
	!word twominus, minusone, over, poke
	!word twominus, lit, 4, over, poke
	!word lit, _source, poke
	!word state, qbranch, evaluate_1, interpret
evaluate_1:
	!word exit

; ==============================================================================
; Colon definition and related words

xcreate_n:
	!byte 8 + NST_FLAG
	!text "(CREATE)"
	!word evaluate_n
xcreate:
	!word call, here, tor
	!word dup, lit, 32, greater, qbranch, xcreate_1, drop, lit, 31 ; limit the word to 31 characters
xcreate_1:
	!word dup, ccomma, rat, oneplus, swap, dup, allot, cmove	; set NFA
	!word latest, comma										; set LFA
	!word rfrom, lit, _latest, poke
	!word lit, created, comma									; set CFA to a special code that pushes PFA on the stack
	!word latest, context, poke, exit							; and add to the search order

create_n:
	!byte 6
	!text "CREATE"
	!word xcreate_n
create:
	!word call, bl, word, count, xcreate, exit

; DOES> is a weird beast. It generates code that will modify the execution of the
; last defined word to jump to the definition word. It is also quite non-portable as it generates a low level instruction
xcode_n:
	!byte 7 + NST_FLAG
	!text "(;CODE)"
	!word create_n
xcode:
	!word call, rfrom										; which is the address of the "call xdoes" instruction
	!word latest, count, lit, NAMEMASK, and_op, add, twoplus	; CFA of the last defined word
	!word poke, exit										; and this will actually exit the defining word

; Note that while DOES> looks like high-level word its implementation is depended on the opcode for native CALL/JSR
doesx_n:
	!byte 5 + IMM_FLAG
	!text "DOES>"
	!word xcode_n
doesx:
	!word call, qcomp, compile, xcode, lit, JSR_INSTR, ccomma, lit, does, comma, exit	; compile (;CODE) followed by "call does_c"

; Note that colon will remove the word from the search order (to be restored by semicolon)
colon_n:
	!byte 1
	!text ":"
	!word doesx_n
colon:
	!word call, create, lit, call, here, twominus, dup, tor, poke, rfrom, twominus, peek, context, poke, bracketx, exit

; Words defined with :NONAME technically don't need to be linked in the vocabulary but if it is done that way RECURSE becomes harder
; to implement. It is easier just to link the word with emtpy name. In this implementation it has an unusual side effect that FIND
; will actually find the last :NONAME if searched for empty string and the test suite actually traps that (not an error though). But -
; standard does not specify it either way; and this is potentially useful.

;
; : :noname here 0 , latest , _latest ! here ' call , ] ;
;
 
colonnoname_n:
	!byte 7
	!text ":NONAME"
	!word colon_n
colonnoname:
	!word call, here, zero, ccomma			; set 0-length NFA
	!word latest, comma, lit, _latest, poke		; LFA
	!word here, lit, call, comma, bracketx, exit	; CFA and keep the address on stack

bufferc_n:
	!byte 7
	!text "BUFFER:"
	!word colonnoname_n
bufferc:
	!word call, create, allot, exit
				
semicolon_n:
	!byte 1 + IMM_FLAG
	!text ";"
	!word bufferc_n
semicolon:
	!word call, qcomp, compile, exit, bracket, latest, context, poke, exit

variable_n:
	!byte 8
	!text "VARIABLE"
	!word semicolon_n
variable:
	!word call, create, zero, comma, exit

twovariable_n:
	!byte 9
	!text "2VARIABLE"
	!word variable_n
twovariable:
	!word call, create, zero, dup, comma, comma, exit

constant_n:
	!byte 8
	!text "CONSTANT"
	!word twovariable_n
constant:
	!word call, create, comma, xcode
	!byte JSR_INSTR
	!word does, peek, exit

defer_n:
	!byte 5
	!text "DEFER"
	!word constant_n
defer:
	!word call, create, lit, dodefer, here, twominus, poke, compile, exit, exit

qdefer_n:
	!byte 6 + NST_FLAG
	!text "?DEFER"
	!word defer_n
qdefer:
	!word call, dup, peek, lit, dodefer, equal, qbranch, qdefer_1, exit
qdefer_1:
	!word xabortq
	!byte 12
	!text "NOT DEFER'ED"

deferpeek_n:
	!byte 6
	!text "DEFER@"
	!word qdefer_n
deferpeek:
	!word call, qdefer, tobody, peek, exit

deferpoke_n:
	!byte 6
	!text "DEFER!"
	!word deferpeek_n
deferpoke:
	!word call, qdefer, tobody, poke, exit

actionof_n:
	!byte 9 + IMM_FLAG
	!text "ACTION-OF"
	!word deferpoke_n
actionof:
	!word call, state, peek, qbranch, actionof_1, btick, compile, deferpeek, exit
actionof_1:
	!word tick, deferpeek, exit

is_n:
	!byte 2 + IMM_FLAG
	!text "IS"
	!word actionof_n
is:
	!word call, state, peek, qbranch, is_1, btick, compile, deferpoke, exit
is_1:
	!word tick, deferpoke, exit

;
; : (comp!) compile lit , compile ! ; nonstandard
; : value create ' dovalue here 2- ! ' >mark , , exit >resolve @ ! (comp!) ;
;

value_n:
	!byte 5
	!text "VALUE"
	!word is_n
value:
	!word call, create, lit, dovalue, here, twominus, poke, lit, value_sem, comma, comma, exit
value_sem:
	!word peek, poke, comppoke
comppoke:
	!word call, compile, lit, comma, compile, poke, exit

to_n:
	!byte 2 + IMM_FLAG
	!text "TO"
	!word value_n
to:
	!word call, bl, word, find, drop, dup, peek, lit, dovalue, equal, qbranch, to_2
	!word twoplus, dup, twoplus, swap, peek, state, peek, qbranch, to_1, twoplus
to_1:
	!word twoplus, peek, execute, exit
to_2:
	!word xabortq
	!byte 11
	!text "NOT A VALUE"

tick_n:
	!byte 1
	!text "'"
	!word to_n
tick:
	!word call, bl, word, find, drop, exit

btick_n:
	!byte 3 + IMM_FLAG
	!text "[']"
	!word tick_n
btick:
	!word call, qcomp, bl, word, find, drop, compile, lit, comma, exit

; This will get the next parameter, compile it to the current definition and skip
compile_n:
	!byte 7 + NST_FLAG
	!text "COMPILE"
	!word btick_n
compile:
	!word call, rfrom, dup, twoplus, tor, peek, comma, exit

bcompile_n:
	!byte 9 + IMM_FLAG
	!text "[COMPILE]"
	!word compile_n
bcompile:
	!word call, qcomp, tick, comma, exit

; Somehow I've managed to get this to pass the tests but I still don't completely understand what
; it is supposed to do
postpone_n:
	!byte 8 + IMM_FLAG
	!text "POSTPONE"
	!word bcompile_n
postpone:
	!word call, qcomp, bl, word, find, one, equal, qbranch, postpone_1, comma, exit
postpone_1:
	!word lit, compile, comma, comma, exit

; This word behaves differently depending on compilation state - in compilation it
; will emit LIT followed by the value from the stack
literal_n:
	!byte 7 + IMM_FLAG
	!text "LITERAL"
	!word postpone_n
literal:
	!word call, qcomp, state, peek, qbranch, literal_1, compile, lit, comma
literal_1:
	!word exit

bracket_n:
	!byte 1 + IMM_FLAG
	!text "["
	!word literal_n
bracket:
	!word call, qcomp, false, state, poke, exit

bracketx_n:
	!byte 1
	!text "]"
	!word bracket_n
bracketx:
	!word call, true, state, poke, exit

;
; : ( source-id 0< if
;     begin ')' parse 2drop >in @ #tib @ = tib #tib @ + 1- c@ ')' = and
;     while refill invert if exit then again
;     else ')' parse 2drop then ; immediate
;

brace_n:
	!byte 1 + IMM_FLAG
	!text "("
	!word bracketx_n
brace:
	!word call, sourceid, zerogt, qbranch, brace_2
brace_1:
	!word lit, ')', parse, twodrop, ptrin, peek, numtib, peek, equal, tib, numtib, peek, add, oneminus, cpeek, lit, ')', notequal, and_op
	!word qbranch, brace_3, refill, invert, qbranch, brace_1, exit
brace_2:
	!word lit, ')', parse, twodrop
brace_3:
	!word exit

backslash_n:
	!byte 1 + IMM_FLAG
	!text "\\"
	!word brace_n
backslash:
	!word call, zero, parse, twodrop, exit

dotbrace_n:
	!byte 2 + IMM_FLAG
	!text ".("
	!word backslash_n
dotbrace:
	!word call, lit, ')', parse, type, exit

commaquote_n:
	!byte 2 + NST_FLAG
	!text ",\""
	!word dotbrace_n
commaquote:
	!word call, lit, '"', parse, dup, ccomma, here, over, allot, swap, cmove, exit

cquote_n:
	!byte 2 + IMM_FLAG
	!text "C\""
	!word commaquote_n
cquote:
	!word call, qcomp, compile, branch, fmark, here, swap, commaquote, fresolve, compile, lit, comma, exit

squote_n:
	!byte 2 + IMM_FLAG
	!text "S\""
	!word cquote_n
squote:
	!word call, state, peek, zerone, qbranch, squote_1, cquote, compile, count, exit
squote_1:
	!word lit, '"', parse, lit, _sbuf, lit, _sflip, peek, lit, 100, mult, add, swap, twotor, tworat, cmove
	!word tworfrom, lit, _sflip, dup, peek, one, xor, swap, poke, exit 

ssquote_n:
	!byte 3 + IMM_FLAG
	!text "S\\\""
	!word squote_n
ssquote:
	!word call, tib, ptrin, peek, add, lit, _sbuf, lit, _sflip, peek, lit, 100, mult, add, numtib, peek, ptrin, peek, sub, over, tor
	!word smove, swap, ptrin, incpoke
	!word rfrom, swap, lit, _sflip, dup, peek, one, xor, swap, poke
	!word state, peek, qbranch, ssquote_1, compile, branch, fmark, here, two, pick, dup, ccomma, allot, swap, fresolve
	!word compile, lit, dup, comma, compile, count, oneplus, swap, cmove 
ssquote_1:
	!word exit

dotquote_n:
	!byte 2 + IMM_FLAG
	!text ".\""
	!word ssquote_n
dotquote:
	!word call, qcomp, cquote, compile, count, compile, type, exit

compilecomma_n:
	!byte 8
	!text "COMPILE,"
	!word dotquote_n
compilecomma:
	!word call, comma, exit

char_n:
	!byte 4
	!text "CHAR"
	!word compilecomma_n
char:
	!word call, bl, word, charplus, cpeek, exit

bcharb_n:
	!byte 6 + IMM_FLAG
	!text "[CHAR]"
	!word char_n
bcharb:
	!word call, compile, lit, bl, word, charplus, cpeek, comma, exit

xabortq_n:
	!byte 8 + NST_FLAG
	!text "(ABORT\")"
	!word bcharb_n
xabortq:
	!word call, rat, count, type, abort, exit

abortq_n:
	!byte 6 + IMM_FLAG
	!text "ABORT\""
	!word xabortq_n
abortq:
	!word call, qcomp, compile, qbranch, fmark, compile, xabortq, commaquote, fresolve, exit

; In optional Programming-Tools word set
forget_n:
	!byte 6
	!text "FORGET"
	!word abortq_n
forget:
	!word call, bl, word, find, zerone, qbranch, forget_1
	!word twominus, dup, peek, dup, lit, _latest, poke, context, poke
	!word lfatonfa, lit, _here, poke, exit
forget_1:
	!word drop, exit

marker_n:
	!byte 6
	!text "MARKER"
	!word forget_n
marker:
	!word call, create, xcode
	!byte JSR_INSTR
	!word does
	!word twominus, twominus, dup, peek, dup, lit, _latest, poke, context, poke
	!word lfatonfa, lit, _here, poke, exit

recurse_n:
	!byte 7 + IMM_FLAG
	!text "RECURSE"
	!word marker_n
recurse:
	!word call, qcomp, latest, count, add, cellplus, comma, exit

; ==============================================================================
; More control structure support

fmark_n:
	!byte 5 + NST_FLAG
	!text ">MARK"
	!word recurse_n
fmark:
	!word call, here, zero, comma, exit

fresolve_n:
	!byte 8 + NST_FLAG
	!text ">RESOLVE"
	!word fmark_n
fresolve:
	!word call, here, swap, poke, exit

rmark_n:
	!byte 5 + NST_FLAG
	!text "<MARK"
	!word fresolve_n
rmark:
	!word call, here, exit

rresolve_n:
	!byte 8 + NST_FLAG
	!text "<RESOLVE"
	!word rmark_n
rresolve:
	!word call, comma, exit

; ==============================================================================
; Control words. All of these are immediate and make don't do anything useful
; in interpreter mode. There should be no code calling to CFA of these words,
; the CFA labels are removed to enforce that
; To understand the concept behind these words look at the BEGIN/AGAIN pair -
; BEGIN ends up just putting RI on the stack and AGAIN compiles BRANCH to that RI.
; Forward references are a bit trickier but follow the same pattern.

begin_n:
	!byte 5 + IMM_FLAG
	!text "BEGIN"
	!word rresolve_n
	!word call, qcomp, rmark, exit

until_n:
	!byte 5 + IMM_FLAG
	!text "UNTIL"
	!word begin_n
	!word call, qcomp, compile, qbranch, rresolve, exit

again_n:
	!byte 5 + IMM_FLAG
	!text "AGAIN"
	!word until_n
	!word call, qcomp, compile, branch, rresolve, exit

if_n:
	!byte 2 + IMM_FLAG
	!text "IF"
	!word again_n
	!word call, qcomp, compile, qbranch, fmark, exit

then_n:
	!byte 4 + IMM_FLAG
	!text "THEN"
	!word if_n
	!word call, qcomp, fresolve, exit

else_n:
	!byte 4 + IMM_FLAG
	!text "ELSE"
	!word then_n
	!word call, qcomp, compile, branch, fmark, swap, fresolve, exit

while_n:
	!byte 5 + IMM_FLAG
	!text "WHILE"
	!word else_n
	!word call, qcomp, compile, qbranch, fmark, swap, exit

repeat_n:
	!byte 6 + IMM_FLAG
	!text "REPEAT"
	!word while_n
	!word call, qcomp, compile, branch, rresolve, fresolve, exit

; DO/LOOP are considerably more complex so they are often implemented with helper words. In practical
; implementations these helpers are done in native code, so are I, J, and LEAVE.
; (DO) stores on the return stack: leaveaddr, limit, current, (ret) 
xdo_n:
	!byte 4 + NST_FLAG
	!text "(DO)"
	!word repeat_n
xdo:
	!word call
	!word rfrom, dup, peek, tor	; forward ref for LEAVE
	!word rot, tor, swap, tor
	!word twoplus, tor				; step over the actual forward ref
	!word exit

xqdo_n:
	!byte 5 + NST_FLAG
	!text "(?DO)"
	!word xdo_n
xqdo:
	!word call
	!word twodup, equal, qbranch, xqdo_1, twodrop, rfrom, peek, tor, exit
xqdo_1:
	!word rfrom, dup, peek, tor	; forward ref for LEAVE
	!word rot, tor, swap, tor
	!word twoplus, tor				; step over the actual forward ref
	!word exit
			
; and (LOOP) adjusts the values on rstack or just drops the top three values from it to exit
xloop_n:
	!byte 6 + NST_FLAG
	!text "(LOOP)"
	!word xqdo_n
xloop:
	!word call, rfrom				; return address is only needed to get the backref
	!word rfrom, oneplus			; new value of current
	!word rat, over, equal, qbranch, xloop_1
	!word drop, drop, rdrop, exit	; exit the loop (leaveaddr on the rstack)
xloop_1:
	!word tor, peek, tor, exit		; continue the loop

xploop_n:
	!byte 7 + NST_FLAG
	!text "(+LOOP)"
	!word xloop_n
xploop:
	!word call, rfrom, swap		; return address is only needed to get the backref / addr, step
	!word dup, rat, add			; preserve step value and get new value of current / addr, step, newcur
	!word rfrom, rat, sub			; diff limit and new current / addr, step, newcur, olddiff
	!word rot, over, xor, zerolt, swap ; new diff and step have different signs? / addr, newcur, step^olddiff<0, olddiff
	!word two, pick, rat, sub		; diff limit and previous current / addr, newcur, s^d, olddiff, newdiff
	!word xor, zerolt, and_op, qbranch, xploop_1  ; or diffs before and after have different signs / newdiff^olddiff < 0
	!word drop, drop, rdrop, exit	; exit the loop (leaveaddr on the rstack)
xploop_1:
	!word tor, peek, tor, exit		; continue the loop

do_n:
	!byte 2 + IMM_FLAG
	!text "DO"
	!word xploop_n
do:
	!word call, qcomp, compile, xdo, fmark, rmark, exit

qdo_n:
	!byte 3 + IMM_FLAG
	!text "?DO"
	!word do_n
qdo:
	!word call, qcomp, compile, xqdo, fmark, rmark, exit

loop_n:
	!byte 4 + IMM_FLAG
	!text "LOOP"
	!word qdo_n
loop:
	!word call, qcomp, compile, xloop, rresolve, fresolve, exit

ploop_n:
	!byte 5 + IMM_FLAG
	!text "+LOOP"
	!word loop_n
ploop:
	!word call, qcomp, compile, xploop, rresolve, fresolve, exit

i_n:
	!byte 1
	!text "I"
	!word ploop_n
i:
	!word call, rfrom, rat, swap, tor, exit

j_n:
	!byte 1
	!text "J"
	!word i_n
j:
	!word call
	!word rfrom, rfrom, rfrom, rfrom, rfrom, dup, tor, swap, tor, swap, tor, swap, tor, swap, tor, exit

leave_n:
	!byte 5
	!text "LEAVE"
	!word j_n
leave:
	!word call, rdrop, rdrop, rdrop, exit
			
unloop_n:
	!byte 6
	!text "UNLOOP"
	!word leave_n
unloop:
	!word call, rfrom, rdrop, rdrop, rdrop, tor, exit

case_n:
	!byte 4 + IMM_FLAG
	!text "CASE"
	!word unloop_n
case:
	!word call, qcomp, depth, rfrom, swap, tor, tor, exit

of_n:
	!byte 2 + IMM_FLAG
	!text "OF"
	!word case_n
of:
	!word call, qcomp, compile, over, compile, equal, compile, qbranch, fmark, compile, drop, exit

endof_n:
	!byte 5 + IMM_FLAG
	!text "ENDOF"
	!word of_n
endof:
	!word call, qcomp, compile, branch, fmark, swap, fresolve, exit

endcase_n:
	!byte 7 + IMM_FLAG
	!text "ENDCASE"
	!word endof_n
endcase:
	!word call, qcomp, compile, drop, depth, rfrom, rfrom, swap, tor, sub
endcase_1:
	!word dup, qbranch, endcase_2, oneminus, swap, fresolve, branch, endcase_1
endcase_2:
	!word drop, exit

; ==============================================================================
; Some nice to have words

spaces_n:
	!byte 6
	!text "SPACES"
	!word endcase_n
spaces:
	!word call
spaces_1:
	!word dup, zerogt, qbranch, spaces_2, oneminus, space, branch, spaces_1
spaces_2:
	!word drop, exit

; In optional String word set
cmove_n:
	!byte 5
	!text "CMOVE"
	!word spaces_n
cmove:
	!word cmove_x
cmove_x:
	+dpop
	+stax _scratch
	+dpop
	+stax _wscratch
	+dpop
	+stax _rscratch
	
	jsr movedown
	jmp next

; In optional String word set
cmovex_n:
	!byte 6
	!text "CMOVE>"
	!word cmove_n
cmovex:
	!word cmovex_x
cmovex_x:
	+dpop
	+stax _scratch
	+dpop
	+stax _wscratch
	+dpop
	+stax _rscratch
	
	jsr moveup
	jmp next

move_n:
	!byte 4
	!text "MOVE"
	!word cmovex_n
move:
	!word call, rot, rot, twodup, less, qbranch, move_1, rot, cmovex, exit
move_1:
	!word rot, cmove, exit

; Non-standard word, similar to CMOVE but does character conversions for S\". Returns number
; of characters processed and returned
smove_n:
	!byte 5 + NST_FLAG
	!text "SMOVE"
	!word move_n
smove:
	!word call, dup, tor, base, peek, tor, zero, tor
smove_1:
	!word dup, qbranch, smove_x, oneminus, two, pick, cpeek, lit, '"', notequal, qbranch, smove_x ; decrement before check for quote is intentional!
	!word tor, tor, dup, oneplus, swap, cpeek, dup, lit, '\\', equal, qbranch, smove_0
	!word drop, rfrom, rfrom, dup, qbranch, smove_x, oneminus
	!word tor, tor, dup, oneplus, swap, cpeek
	!word dup, lit, 'A', equal, qbranch, smove_2, drop, lit, 7, branch, smove_0
smove_2:
	!word dup, lit, 'B', equal, qbranch, smove_3, drop, lit, 8, branch, smove_0
smove_3:
	!word dup, lit, 'E', equal, qbranch, smove_4, drop, lit, 27, branch, smove_0
smove_4:
	!word dup, lit, 'F', equal, qbranch, smove_5, drop, lit, 12, branch, smove_0
smove_5:
	!word dup, lit, 'L', equal, qbranch, smove_6, drop, lit, 10, branch, smove_0
smove_6:
	!word dup, lit, 'N', equal, qbranch, smove_7, drop, lit, 13, branch, smove_0
smove_7:
	!word dup, lit, 'Q', equal, qbranch, smove_8, drop, lit, 34, branch, smove_0
smove_8:
	!word dup, lit, 'R', equal, qbranch, smove_9, drop, lit, 13, branch, smove_0
smove_9:
	!word dup, lit, 'T', equal, qbranch, smove_10, drop, lit, 9, branch, smove_0
smove_10:
	!word dup, lit, 'V', equal, qbranch, smove_11, drop, lit, 11, branch, smove_0
smove_11:
	!word dup, lit, 'Z', equal, qbranch, smove_12, drop, zero, branch, smove_0
smove_12:
	!word dup, lit, 'M', equal, qbranch, smove_13, drop
	!word rfrom, lit, 13, over, cpoke, oneplus, lit, 10, over, cpoke, oneplus
	!word rfrom, rfrom, twoplus, tor, branch, smove_1
smove_13:
	!word dup, lit, 'X', equal, qbranch, smove_0, drop
	!word rfrom, rfrom, dup, one, greater, qbranch, smove_x, twominus, tor, tor
	!word hex, dup, twoplus, swap, zero, zero, rot, two, tonumber, qbranch, smove_14
	!word twodrop, twodrop, rdrop, rdrop, rfrom, rfrom, base, poke, exit
smove_14:
	!word twodrop
smove_0:
	!word rfrom, swap, over, cpoke, oneplus, rfrom, rfrom, oneplus, tor, branch, smove_1 
smove_x:
	!word nip, nip, rfrom, rfrom, base, poke, rfrom, rot, sub, swap, exit

pad_n:
	!byte 3
	!text "PAD"
	!word smove_n
pad:
	!word doconst, _pad

unused_n:
	!byte 6
	!text "UNUSED"
	!word pad_n
unused:
	!word call, lit, MEMTOP, here, sub, exit

fill_n:
	!byte 4
	!text "FILL"
	!word unused_n
fill:
	!word call, swap, tor, swap
fill_1:
	!word rfrom, dup, qbranch, fill_2, oneminus, tor, twodup, cpoke, oneplus, branch, fill_1
fill_2:
	!word twodrop, drop, exit

erase_n:
	!byte 5
	!text "ERASE"
	!word fill_n
erase:
	!word call, zero, fill, exit

sstring_n:
	!byte 7
	!text "/STRING"
	!word erase_n
sstring:
	!word call, rot, over, add, rot, rot, sub, exit

blank_n:
	!byte 5
	!text "BLANK"
	!word sstring_n
blank:
	!word call, bl, fill, exit

sliteral_n:
	!byte 8 + IMM_FLAG
	!text "SLITERAL"
	!word blank_n
sliteral:
	!word call, state, peek, qbranch, sliteral_1, compile, branch, fmark, rot, rot
	!word dup, tor, here, dup, tor, swap, dup, allot, cmove, fresolve
	!word compile, lit, rfrom, comma, compile, lit, rfrom, comma
sliteral_1:
	!word exit

qmark_n:
	!byte 1
	!text "?"
	!word sliteral_n
qmark:
	!word call, peek, dot, exit

dots_n:
	!byte 2
	!text ".S"
	!word qmark_n
dots:
	!word call, depth
dots_1:
	!word dup, qbranch, dots_2, dup, pick, dot, oneminus, branch, dots_1
dots_2:
	!word drop, exit

ahead_n:
	!byte 5
	!text "AHEAD"
	!word dots_n
ahead:
	!word call, fmark, exit

; The next two are non-standard but proposed for inclusion
place_n:
	!byte 5 + NST_FLAG
	!text "PLACE"
	!word ahead_n
place:
	!word call, twodup, twotor, charplus, swap, chars, move, tworfrom, cpoke, exit

plusplace_n:
	!byte 6 + NST_FLAG
	!text "+PLACE"
	!word place_n
plusplace:
	!word call, dup, count, add, tor, twodup, cpeek, add, swap, cpoke, rfrom, swap, move, exit

; ==============================================================================
; More words from the optional Double-Number word set

;
; : d= rot = >r = r> and ;
;

dequal_n:
	!byte 2
	!text "D="
	!word plusplace_n
dequal:
	!word call, rot, equal, tor, equal, rfrom, and_op, exit

;
; : dmax 2over 2over d< if 2swap then 2drop ;
; : dmin 2over 2over d< invert if 2swap then 2drop ;
;

dmax_n:
	!byte 4
	!text "DMAX"
	!word dequal_n
dmax:
	!word call, twoover, twoover, dless, qbranch, dmax_1, twoswap
dmax_1:
	!word twodrop, exit

dmin_n:
	!byte 4
	!text "DMIN"
	!word dmax_n
dmin:
	!word call, twoover, twoover, dless, invert, qbranch, dmin_1, twoswap
dmin_1:
	!word twodrop, exit

;
; : d- dnegate d+ ;
; code d+
;

dsub_n:
	!byte 2
	!text "D-"
	!word dmin_n
dsub:
	!word call, dnegate, dadd, exit

wlow = _scratch
whigh = _rscratch

dadd_n:
	!byte 2
	!text "D+"
	!word dsub_n
dadd:
	!word dadd_c
dadd_c:
	+dpop
	+stax whigh
	+dpop
	+stax wlow
	+dpop
	tay
	clc
	lda _dtop
	adc wlow
	sta _dtop
	lda _dtop+1
	adc wlow+1
	sta _dtop+1
	tya
	adc whigh
	tay
	txa
	adc whigh+1
	tax
	tya
	+dpush
	jmp next
	
;	!word call, tor, swap, tor, dup, tor, add, dup, rfrom, uless, one, and_op, rfrom, rfrom, add, add, exit

dtwodiv_n:
	!byte 3
	!text "D2/"
	!word dadd_n
dtwodiv:
	!word call, dup, one, and_op, lit, 15, lshift, swap, twodiv, swap, rot, twodiv, or, swap, exit

;
; : d2* 2dup d+ ;
;

dtwomul_n:
	!byte 3
	!text "D2*"
	!word dtwodiv_n
dtwomul:
	!word call, twodup, dadd, exit

duless_n:
	!byte 3
	!text "DU<"
	!word dtwomul_n
duless:
	!word call, rot, twodup, equal, qbranch, duless_1, twodrop, uless, exit
duless_1:
	!word ugreater, qbranch, duless_2, twodrop, true, exit
duless_2:
	!word twodrop, false, exit

;
; : d0= or 0= ;
; : d0< nip 0< ;
; : d< rot > if 2drop true else < then ;
;

dzeroeq_n:
	!byte 3
	!text "D0="
	!word duless_n
dzeroeq:
	!word call, or, zeroeq, exit

dzeroless_n:
	!byte 3
	!text "D0<"
	!word dzeroeq_n
dzeroless:
	!word call, nip, zerolt, exit

dless_n:
	!byte 2
	!text "D<"
	!word dzeroless_n
dless:
	!word call, rot, twodup, equal, qbranch, dless_1, twodrop, uless, exit
dless_1:
	!word greater, qbranch, dless_2, twodrop, true, exit
dless_2:
	!word twodrop, false, exit

;
; : d>s drop ;
;

dtos_n:
	!byte 3
	!text "D>S"
	!word dless_n
dtos:
	!word call, drop, exit

;
; : 2constant create , , does> 2@ ;
;

dconstant_n:
	!byte 9
	!text "2CONSTANT"
	!word dtos_n
dconstant:
	!word call, create, comma, comma, xcode
	!byte JSR_INSTR
	!word does, twopeek, exit

;
; : 2lit r@ 2@ r> 2+ 2+ >r ; nonstandard
; : 2literal ?comp state @ if compile 2lit , , then ; immediate
;

dlit_n:
	!byte 4 + NST_FLAG
	!text "2LIT"
	!word dconstant_n
dlit:
	!word call, rat, twopeek, rfrom, twoplus, twoplus, tor, exit

dliteral_n:
	!byte 8 + IMM_FLAG
	!text "2LITERAL"
	!word dlit_n
dliteral:
	!word call, qcomp, state, peek, qbranch, dliteral_1, compile, dlit, comma, comma
dliteral_1:
	!word exit

;
; : 2rot 5 roll 5 roll ;
;

drot_n:
	!byte 4
	!text "2ROT"
	!word dliteral_n
drot:
	!word call, lit, 5, roll, lit, 5, roll, exit

dvalue_n:
	!byte 6
	!text "2VALUE"
	!word drot_n
dvalue:
	!word call, create, lit, dovalue, here, twominus, poke, lit, dvalue_sem, comma, comma, comma, exit
dvalue_sem:
	!word twopeek, twopoke, compdpoke
compdpoke:
	!word call, compile, lit, comma, compile, twopoke, exit

;
; M*/ is an unusual word that uses three-cell numbers. It is possible to build it from the existing words
; To make it more clear, using some internal helpers:
; : t* ( ud,u -- ut) 2>r r@ m* 0 2r> m* d+ ;
; : t/ ( ut,u -- ud) dup >r um/mod r> swap >r um/mod nip r> ;
; : normsign ( d,n -- ud,u,n ) 2dup xor >r abs rot rot dabs rot r> ;
;
tmult:
	!word call, twotor, rat, ummult, zero, tworfrom, ummult, dadd, exit
tdiv:
	!word call, dup, tor, ummod, rfrom, swap, tor, ummod, nip, rfrom, exit
normsign:
	!word call, twodup, xor, tor, abs, rot, rot, dabs, rot, rfrom, exit

;
; : m*/ >r normsign r> swap >r >r t* r> t/ r> 0< if dnegate then ;
;

mmuldiv_n:
	!byte 3
	!text "M*/"
	!word dvalue_n
mmuldiv:
	!word call, tor, normsign, rfrom, swap, tor, tor, tmult, rfrom, tdiv, rfrom, zerolt, qbranch, mmuldiv_1, dnegate
mmuldiv_1:
	!word exit

; ==============================================================================

env_counted_n:
	!byte 15 + NST_FLAG
	!text "/COUNTED-STRING"
	!word mmuldiv_n
	!word doconst, 255
				
env_hold_n:
	!byte 5 + NST_FLAG
	!text "/HOLD"
	!word env_counted_n
	!word doconst, 98
				
env_pad_n:
	!byte 4 + NST_FLAG
	!text "/PAD"
	!word env_hold_n
	!word doconst, 100
				
env_bits_n:
	!byte 17 + NST_FLAG
	!text "ADDRESS-UNIT-BITS"
	!word env_pad_n
	!word doconst, 16
				
env_floored_n:
	!byte 7 + NST_FLAG
	!text "FLOORED"
	!word env_bits_n
	!word call, true, exit
				
env_maxchar_n:
	!byte 8 + NST_FLAG
	!text "MAX-CHAR"
	!word env_floored_n
	!word doconst, 255
				
env_maxd_n:
	!byte 5 + NST_FLAG
	!text "MAX-D"
	!word env_maxchar_n
	!word call, dlit, $FFFF, $7FFF, exit
				
env_maxn_n:
	!byte 5 + NST_FLAG
	!text "MAX-N"
	!word env_maxd_n
	!word doconst, $7FFF
				
env_maxu_n:
	!byte 5 + NST_FLAG
	!text "MAX-U"
	!word env_maxn_n
	!word doconst, $FFFF
				
env_maxud_n:
	!byte 6 + NST_FLAG
	!text "MAX-UD"
	!word env_maxu_n
	!word call, dlit, $FFFF, $FFFF, exit
				
env_rstack_n:
	!byte 18 + NST_FLAG
	!text "RETURN-STACK-CELLS"
	!word env_maxud_n
	!word doconst, RSIZE / 2
				
env_stack_n:
	!byte 11 + NST_FLAG
	!text "STACK-CELLS"
	!word env_rstack_n
	!word doconst, (DSIZE - 4*SSAFE) / 2 + 1	; Note the extra cell for _dtop
				
environmentq_n:
	!byte 12
	!text "ENVIRONMENT?"
	!word env_stack_n
environmentq:
	!word call, xfind, dup, qbranch, environmentq_1, count, lit, NAMEMASK, and_op, add, twoplus, execute, true
environmentq_1:
	!word exit

; ==============================================================================
; Optional File-Access word set

; Forth standard makes assumptions about I/O capabilities that are simply not true
; for most 8-bit systems. Implementing as much as possible to get the system going

_devnum = _rscratch
_secondary = _wscratch

; Full equivalent to C64 OPEN, not exposed to dictionary yet
; Note that it requires 5 stack parameters as the string is enchoded as (c_addr,u)
; Top of data stack will have filenum or 0 on error
c64open:
	!word c64open_x
c64open_x:
	+dpop
	ldx _dtop
	ldy _dtop+1
	jsr SETNAM
	+dpop
	lda _dtop
	sta _secondary
	+dpop
	lda _dtop
	sta _devnum
	+dpop
	lda _dtop
	ldx _devnum
	ldy _secondary
	jsr SETLFS
	jsr OPEN
	bcs c64open_error	; TODO: not seeing this firing
	jsr READST
	bne c64open_error
	jmp next
c64open_error:
	lda _dtop
	jsr CLOSE
	lda #0
	sta _dtop
	jmp next

; Corresponding equivalent to CLOSE
c64close:
	!word c64close_x
c64close_x:
	+dpop
	jsr CLOSE
	jmp next


bin_n:
	!byte 3
	!text "BIN"
	!word environmentq_n
bin:
	!word call, exit		; taking the recommendation and handling all files as binary

ro_n:
	!byte 3
	!text "R/O"
	!word bin_n
ro:
	!word doconst, ro_v
ro_v:
	!byte 4
	!text ",S,R"

wo_n:
	!byte 3
	!text "W/O"
	!word ro_n
wo:
	!word doconst, wo_v
wo_v:
	!byte 4
	!text ",S,W"

; This may not be supported on C64, making it identical to W/O
rw_n:
	!byte 3
	!text "R/W"
	!word wo_n
rw:
	!word doconst, wo_v

; For C64 OPEN-FILE and CREATE-FILE are identical
createfile_n:
	!byte 11
	!text "CREATE-FILE"
	!word rw_n
createfile:
	!word call, openfile, exit

openfile_n:
	!byte 9
	!text "OPEN-FILE"
	!word createfile_n
openfile:
	!word call, tor, lit, of_1, count, lit, _fnamebuf, place, lit, _fnamebuf, plusplace
	!word rfrom, count, lit, _fnamebuf, plusplace, lit, _openfiles, peek, freebit
	!word dup, zerone, qbranch, of_2, lit, 8, over, lit, _fnamebuf, count, c64open, dup, lit, _openfiles, setbit, dup, zeroeq
of_2:
	!word exit
of_1:
	!byte 3
	!text "O0:"

closefile_n:
	!byte 10
	!text "CLOSE-FILE"
	!word openfile_n
closefile:
	!word call, dup, lit, _openfiles, clearbit, c64close, zero, exit

; C64 equivalent: OPEN 1,8,15,"S0:Name":CLOSE 1
deletefile_n:
	!byte 11
	!text "DELETE-FILE"
	!word closefile_n
deletefile:
	!word call, lit, df_1, count, lit, _fnamebuf, place, lit, _fnamebuf, plusplace
	!word one, lit, 8, lit, 15, lit, _fnamebuf, count, c64open
	!word one, notequal, one, c64close, exit
df_1:
	!byte 3
	!text "S0:"

; C64 equivalent: OPEN 1,8,15,"R0:NewName=OldName":CLOSE 1
renamefile_n:
	!byte 11
	!text "RENAME-FILE"	; Note that this is the only word that uses PAD
	!word deletefile_n
renamefile:
	!word call, lit, rf_1, count, lit, _fnamebuf, place
	!word lit, _fnamebuf, plusplace
	!word lit, rf_2, count, lit, _fnamebuf, plusplace
	!word lit, _fnamebuf, plusplace
	!word one, lit, 8, lit, 15, lit, _fnamebuf, count, c64open
	!word one, notequal, one, c64close, exit
rf_1:
	!byte 3
	!text "R0:"
rf_2:
	!byte 1
	!text "="

; Cannot be implemented on C64
resizefile_n:
	!byte 11
	!text "RESIZE-FILE"
	!word renamefile_n
resizefile:
	!word call, drop, twodrop, minusone, exit

; Cannot be implemented on C64
repositionfile_n:
	!byte 15
	!text "REPOSITION-FILE"
	!word resizefile_n
repositionfile:
	!word call, drop, twodrop, minusone, exit

; Cannot be implemented on C64
fileposition_n:
	!byte 13
	!text "FILE-POSITION"
	!word repositionfile_n
fileposition:
	!word call, drop, zero, zero, minusone, exit

; Cannot be implemented on C64
filesize_n:
	!byte 9
	!text "FILE-SIZE"
	!word fileposition_n
filesize:
	!word call, drop, zero, zero, minusone, exit

; A simplistic implementation to test for existence
filestatus_n:
	!byte 11
	!text "FILE-STATUS"
	!word filesize_n
filestatus:
	!word call, ro, openfile, qbranch, filestatus_1, true, exit
filestatus_1:
	!word closefile, zero, exit

setread:
	!word setread_c
setread_c:
	+dpop
	tax
	jsr CHKIN
	jmp next

xreadchar:
	!word xreadchar_c
xreadchar_c:
	jsr CHRIN
	ldx #0
	and #$7F		; Ignore high bit (so Shift-Space is not a problem)
	cmp #10			; Do two substitutions: \n -> \r and \t -> ' '
	bne xreadchar_1
	lda NEW_LINE
xreadchar_1:
	cmp #9
	bne xreadchar_2
	lda #32
xreadchar_2:
	+dpush
	jmp next

xreadbyte:
	!word xreadbyte_c
xreadbyte_c:
	jsr CHRIN
	ldx #0
	+dpush
	jmp next

iseof:
	!word iseof_c
iseof_c:
	jsr READST
	and #64
	tax
	+dpush
	jmp next

; There is something odd in EOF logic on X16 (maybe on C64 as well). The EOF state is not kept with
; the handle properly, so it will be false after CHRIN triggering false reads on the line after
; the EOF (the line with EOF cannot really return that state per Forth standard).
; As a workaround, skipping zero chars in READ-LINE, these should not occur in text files anyway.

readline_n:
	!byte 9
	!text "READ-LINE"
	!word filestatus_n
readline:
	!word call, setread, swap, dup, rot, add, over					; c-addr, c-addr-limit, current
readline_1:
	!word twodup, swap, uless, qbranch, readline_4				; buffer full?
readline_7:
	!word iseof, zeroeq, qbranch, readline_2, xreadchar, qdup, qbranch, readline_7 ; EOF workaround
	!word dup, lit, NEW_LINE, notequal, qbranch, readline_3		; end of line
	!word over, cpoke, oneplus, branch, readline_1 
readline_2:
	!word swap, drop, swap, sub, dup, zeroeq, qbranch, readline_5, false, branch, readline_6
readline_3:
	!word drop
readline_4:
	!word swap, drop, swap, sub
readline_5:
	!word true
readline_6:
	!word zero, setread, zero, exit

readfile_n:
	!byte 9
	!text "READ-FILE"
	!word readline_n
readfile:
	!word call, setread, swap, dup, rot, add, over					; c-addr, c-addr-limit, current
readfile_1:
	!word twodup, swap, uless, qbranch, readfile_3				; buffer full?
	!word iseof, zeroeq, qbranch, readfile_2, xreadbyte		; end of file?
	!word over, cpoke, oneplus, branch, readfile_1 
readfile_2:
readfile_3:
	!word swap, drop, swap, sub, zero, zero, setread, exit

setwrite:
	!word setwrite_c
setwrite_c:
	+dpop
	tax
	jsr CHKOUT
	jmp next

xputchar = emit

writeline_n:
	!byte 10
	!text "WRITE-LINE"
	!word readfile_n
writeline:
	!word call, dup, tor, writefile, rfrom, setwrite, lit, NEW_LINE, xputchar, zero, setwrite, exit

; This can be implemented using KERNAL SAVE, but the corresponding READ-LINE cannot be implemented
; with KERNAL LOAD. Leaving it as is for now.
writefile_n:
	!byte 10
	!text "WRITE-FILE"
	!word writeline_n
writefile:
	!word call, setwrite
writefile_1:
	!word dup, zerone, qbranch, writefile_2, swap, dup, cpeek, xputchar, oneplus, swap, oneminus, branch, writefile_1
writefile_2:
	!word twodrop, zero, setwrite, zero, exit

; Not needed on C64
flushfile_n:
	!byte 10
	!text "FLUSH-FILE"
	!word writefile_n
flushfile:
	!word call, zero, exit

includefile_n:
	!byte 12
	!text "INCLUDE-FILE"
	!word flushfile_n
includefile:
	!word call, lit, _ibufcount, peek, lit, 7, greater, qbranch, includefile_1, xabortq
	!byte 17
	!text "TOO MANY INCLUDES"
includefile_1:
	!word lit, _source, peek
	!word twominus, zero, over, poke	; two more entries to keep fileposition before the last refill
	!word twominus, zero, over, poke
	!word twominus, zero, over, poke
	!word twominus, lit, _ibuf, lit, _ibufcount, peek, lit, 100, mult, add, over, poke
	!word twominus, zero, over, poke
	!word twominus, swap, over, poke
	!word twominus, lit, 6, over, poke
	!word lit, _source, poke
	!word lit, _ibufcount, dup, peek, oneplus, swap, poke
	!word state, qbranch, includefile_2, interpret
includefile_2:
	!word exit

include_n:
	!byte 7
	!text "INCLUDE"
	!word includefile_n
include:
	!word call, parsename, included, exit

; TODO - should produce an error on non-existing file. Currently OPEN-FILE does not report that properly
included_n:
	!byte 8
	!text "INCLUDED"
	!word include_n
included:
	!word call, twodup, filestatus, nip, qbranch, included_1, twodrop, exit
included_1:
	!word twodup, xcreate, ro, openfile, qbranch, included_2, drop, exit
included_2:
	!word includefile, exit

require_n:
	!byte 7
	!text "REQUIRE"
	!word included_n
require:
	!word call, parsename, required, exit

required_n:
	!byte 8
	!text "REQUIRED"
	!word require_n
required:
	!word call, twodup, xfind, zeroeq, qbranch, required_1, included, exit
required_1:
	!word twodrop, exit

; ==============================================================================
; Small subset from the optional Facility word set

beginstructure_n:
	!byte 15
	!text "BEGIN-STRUCTURE"
	!word required_n
	!word call, create, here, zero, zero, comma, xcode
	!byte JSR_INSTR
	!word does, peek, exit

endstructure_n:
	!byte 13
	!text "END-STRUCTURE"
	!word beginstructure_n
	!word call, swap, poke, exit
				
addfield_n:
	!byte 6
	!text "+FIELD"
	!word endstructure_n
addfield:
	!word call, create, over, comma, add, xcode
	!byte JSR_INSTR
	!word does, peek, add, exit
			
field_n:
	!byte 6
	!text "FIELD:"
	!word addfield_n
	!word call, two, addfield, exit

cfield_n:
	!byte 7
	!text "CFIELD:"
	!word field_n
	!word call, one, addfield, exit

; ==============================================================================
; The main system loop. Intentionally placing it last so the listing of words will start with it

forth_system_n:
	!byte 12 + NST_FLAG
	!text "FORTH-SYSTEM"
	!word cfield_n
forth_system:
	!word call
forth_system_c:
	!word lit, banner_text, count, type, cr
	!word decimal, false, state, poke, xsst
	!word lit, autorun, count, included, branch, forth_system_1
forth_system_r:
	!word decimal, false, state, poke, xsst
forth_system_1:
	!word interpret, branch, forth_system_1
banner_text:
	!byte 14
	!text "FORTH TX16 1.0"
autorun:
	!byte 11
	!text "AUTORUN.FTH"

; ==============================================================================

end_of_image:

