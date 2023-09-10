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
JMP_INSTR = $4C ; in direct threading each Forth word stars with JMP CALL

NEW_LINE = $0D

USER_BASE	= $22
_ri			= USER_BASE		; inner interpreter registers
_w			= _ri+2
_rstack		= _w+2			; stack pointers
_dstack		= _rstack+2
_dtop		= _dstack+2		; the very top element of the data stack
_rscratch	= _dtop+2		; four scratch registers used in multiple algorithms (sometimes aliased)
_wscratch	= _rscratch+2
_scratch	= _wscratch+2
_scratch_1	= _scratch+2
_scratch_2	= _scratch_1+2

; Other zero page vars
_here		= _scratch_2+2
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

!macro inc16 .addr {
	inc .addr
	bne +
	inc .addr+1
+
}

!macro dec16 .addr {
	pha
	lda .addr
	bne +
	dec .addr+1
+	dec .addr
	pla
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

; Elements of Forth word definition

; Complete word header including name, flags, and links
!macro header ~.label, ~.label_n, .prev_n, .s, .name {
.label_n:
	!byte .s
	!text .name
	!word .prev_n
.label:
}

; Internal words (native code inserts) that should not show in the vocabulary.
; There are two reasons for this: to properly identify tokens and to allow
; decompilation in the future.
; Anything that starts with +code or +forth should have one of the header
; varieties.
!macro header_internal ~.label {
	!byte 1
	!text "~"
	!word 0
.label:
}

; Beginning of a compiled Forth word. Tokens follow
!macro forth {
	jmp call
}

; Beginning of a native code word. Assembler codes follow
!macro code {
}

; Passing execution to native code elsewhere
!macro code .a {
	jmp .a
}

; 16-bit value (typically for preceeding LIT token). Can be a native address
!macro value .v {
	!word .v
}

; Address (typically for preceeding BRANCH or ?BRANCH tokens)
!macro address .a {
	!word .a
}

; Pascal-style string
!macro string .l, .s {
	!byte .l
	!text .s
}

; One to eight tokens per line
!macro token .t1 {
	!word .t1
}

!macro token .t1, .t2 {
	!word .t1, .t2
}

!macro token .t1, .t2, .t3 {
	!word .t1, .t2, .t3
}

!macro token .t1, .t2, .t3, .t4 {
	!word .t1, .t2, .t3, .t4
}

!macro token .t1, .t2, .t3, .t4, .t5 {
	!word .t1, .t2, .t3, .t4, .t5
}

!macro token .t1, .t2, .t3, .t4, .t5, .t6 {
	!word .t1, .t2, .t3, .t4, .t5, .t6
}

!macro token .t1, .t2, .t3, .t4, .t5, .t6, .t7 {
	!word .t1, .t2, .t3, .t4, .t5, .t6, .t7
}

!macro token .t1, .t2, .t3, .t4, .t5, .t6, .t7, .t8 {
	!word .t1, .t2, .t3, .t4, .t5, .t6, .t7, .t8
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

; Structure of a vocabulary word in direct threaded code:
; offset     length      meaning
;    0          1        n - length of the name and flags (NFA)
;    1          n        name
;    n+1        2        link to the previous word (LFA)
;    n+3        3        jump to code (typically CALL) (CFA)
;    n+6        x        parameter list (for CALL these often are pointers
;                          to code fields of other words (PFA)
; Note that the CPU architecture does not require alignment. If it was, the LFA
; could be aligned
; Most elements of a word definition are hidden behind macros. Note the particular
; distinction between token, value, and address. Token size may change with the
; interpreter threading model; the address may potentially become short relative in
; some cases. Note that each work definition starts either with +forth or +code
; Example:
; Forth   : Example 2 dup + . ;
; Assembly:
; +header ~example, ~example_n, link_last_n, 6, "Example"
;            +forth
;            +token lit
;            +value 2
;            +token dup, plus, dot, exit

; ==============================================================================
; Inner interpreter
;
; To start the interpreter, RI needs to point to CFA of the first word to execute and NEXT should be executed

; NEXT - execute the word at RI (RI is pointer to CFA), prime the parameter pointer for CALL
;	W = mem(RI)
;	RI += 2
;	goto mem(W)

next:
	ldy #0
	lda (_ri),y
	sta _w
	iny
	lda (_ri),y
	sta _w+1
	
	lda _ri
	+add 2
	sta _ri
	bcc jmponw
	inc _ri+1

jmponw:
	jmp (_w)

; CALL - this will execute the parameters at W
; 	rpush(RI)
;	RI = W+3	; offset for JMP call
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
	+incax 3
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
;	push(W+3)	; offset for jmp call
;	goto NEXT

created:
	+ldax _w
	+incax 3
	+dpush
	jmp next

; DOES - semantics applied to the defined word by DOES>. It is an extension of CREATE semantics that redirects
; the execution to the creating word
;	rpush(RI)
;	RI = pop()	; this is supposed to be the return address from the SUB (JSR) instruction, not the top of the Forth stack!
;	push(W+3)	; offset for jmp call
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
;	W = mem(W+3)	; offset for jmp call
;	goto mem(W)

dodefer:
	ldy #4
	lda (_w),y
	tax
	dey
	lda (_w),y
	+stax _w
	jmp jmponw

; DOVALUE - semantics of a VALUE
;	Assumes a very particular structure: pointer to semantics block followed by value bytes. Semantics block contains
;	three addresses: read semantics, write semantics, compilation semantics
;	push(W+5)			; offset for jmp call and one pointer to semantics block
;	W = mem(mem(W+3))	; offset for jmp call
;	goto mem(W)

dovalue:
	+ldax _w
	+incax 5
	+dpush
	ldy #4
	lda (_w),y
	sta _rscratch+1
	dey
	lda (_w),y
	sta _rscratch
	ldy #1
	lda (_rscratch),y
	sta _w+1
	dey
	lda (_rscratch),y
	sta _w
	jmp jmponw

; A number of Forth words have constant semantics. Typical systems define CONSTANT using DOES> but that wastes a few
; bytes for the call. Using a separate semantic word instead.
;	push mem(W+3)	; offset for jmp call
;	goto NEXT

doconst:
	ldy #4
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

+header ~bye, ~bye_n, 0, 3, "BYE"
	+code
	rts

; EXIT is used to return from any word.
+header ~exit, ~exit_n, bye_n, 4, "EXIT"
	+code return

; Execute the word by address on the stack
+header ~execute, ~execute_n, exit_n, 7, "EXECUTE"
	+code invoke

; Reset return stack, dispose of sources, close all open files, and reenter the system.
+header ~quit, ~quit_n, execute_n, 4, "QUIT"
	+forth
	+token xsst
	+token xquit ; this is an equivalent to ;CODE
xquit:
	+code
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

; TODO: should also call (SST) in QUIT
; Reset data stack and perform QUIT.
+header ~abort, ~abort_n, quit_n, 5, "ABORT"
	+code
abort_c:
	+init_dstack
	jmp quit_c

;
; : (sst) _sourcestack
;         2- 0 over ! 2- _tib over ! 2- 0 over ! 2- 0 over ! 2- 4 over !
;         _source ! 0 dup _sflip ! _ibufcount ! ; nonstandard
;

+header ~xsst, ~xsst_n, abort_n, 5 + NST_FLAG, "(SST)"	; Reset source stack
	+forth
	+token lit
	+value _sourcestack
	+token twominus, zero, over, poke			; #TIB
	+token twominus, lit
	+value _tib
	+token over, poke							; TIB
	+token twominus, zero, over, poke			; >IN
	+token twominus, zero, over, poke			; SOURCE-ID
	+token twominus, lit
	+value $0004
	+token over, poke		; standard input has 4 parameters: 0, >IN, TIB, #TIB
	+token lit
	+value _source
	+token poke
	+token zero, dup, lit
	+value _sflip
	+token poke, lit
	+value _ibufcount
	+token poke, exit

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

+header ~xinits, ~xinits_n, xsst_n, 7 + NST_FLAG, "(INITS)"
	+code created
xinit_startup:
	+address forth_system_c
xinit_here:
	+address end_of_image
xinit_context:
	+address forth_system_n
xinit_latest:
	+address forth_system_n

; SAVE-SYSTEM has been modified for X16 version - the image start is not 0
; anymore and the image requires the image start to be written to the file first
+header ~savesystem, ~savesystem_n, xinits_n, 11 + NST_FLAG, "SAVE-SYSTEM"
	+forth
	+token parsename, wo, openfile, zeroeq, qbranch
	+address savesystem_1
	+token tor
	+token lit
	+value start_of_image
	+token pad, poke, pad, two, rat, writefile
	+token lit
	+value start_of_image
	+token lit
	+value xinit_startup
	+token over, sub, rat, writefile, drop
	+token pad, lit
	+value forth_system_c
	+token over, poke
	+token twoplus, here, over, poke
	+token twoplus, lit
	+value _context
	+token peek, over, poke
	+token twoplus, lit
	+value _latest
	+token peek, over, poke
	+token drop, pad, lit
	+value $0008
	+token rat, writefile, drop
	+token lit
	+value xinit_latest
	+token twoplus, here, over, sub, rat, writefile, drop
	+token rfrom, closefile
savesystem_1:
	+token drop, exit


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

+header ~zero, ~zero_n, savesystem_n, 1 + NST_FLAG, "0"
	+code doconst
	+value $0000

+header ~one, ~one_n, zero_n, 1 + NST_FLAG, "1"
	+code doconst
	+value $0001

+header ~two, ~two_n, one_n, 1 + NST_FLAG, "2"
	+code doconst
	+value $0002

+header ~minusone, ~minusone_n, two_n, 2 + NST_FLAG, "-1"
	+code doconst
	+value $FFFF

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

+header ~add, ~add_n, minusone_n, 1, "+"
	+code
	+dpop
	clc
	adc _dtop
	sta _dtop
	txa
	adc _dtop+1
	sta _dtop+1
	jmp next

+header ~sub, ~sub_n, add_n, 1, "-"
	+code
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

+header ~mult, ~mult_n, sub_n, 1, "*"
	+forth
	+token mmult, drop, exit
		
+header ~div, ~div_n, mult_n, 1, "/"
	+forth
	+token divmod, nip, exit

+header ~mod, ~mod_n, div_n, 3, "MOD"
	+forth
	+token divmod, drop, exit

+header ~divmod, ~divmod_n, mod_n, 4, "/MOD"
	+forth
	+token tor, stod, rfrom, smrem, exit

+header ~multdivmod, ~multdivmod_n, divmod_n, 5, "*/MOD"
	+forth
	+token tor, mmult, rfrom, smrem, exit

+header ~multdiv, ~multdiv_n, multdivmod_n, 2, "*/"
	+forth
	+token multdivmod, nip, exit

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

+header ~abs, ~abs_n, multdiv_n, 3, "ABS"
	+code
	lda _dtop+1
	bmi negate_c
	jmp next

+header ~negate, ~negate_n, abs_n, 6, "NEGATE"
	+code
negate_c:
	lda #0
	sec
	sbc _dtop
	sta _dtop
	lda #0
	sbc _dtop+1
	sta _dtop+1
	jmp next

+header ~oneplus, ~oneplus_n, negate_n, 2, "1+"
	+code
	inc _dtop
	bne oneplus_1
	inc _dtop+1
oneplus_1:
	jmp next

+header ~oneminus, ~oneminus_n, oneplus_n, 2, "1-"
	+code
	lda _dtop
	bne oneminus_1
	dec _dtop+1
oneminus_1:
	dec _dtop
	jmp next

+header ~twoplus, ~twoplus_n, oneminus_n, 2 + NST_FLAG, "2+"
	+code
	+ldax _dtop
	+incax 2
	+stax _dtop
	jmp next

+header ~twominus, ~twominus_n, twoplus_n, 2 + NST_FLAG, "2-"
	+code
	+ldax _dtop
	+decax 2
	+stax _dtop
	jmp next

+header ~twodiv, ~twodiv_n, twominus_n, 2, "2/"
	+code
	lda _dtop+1
	cmp #$80		; 6502 does not have native arithmetic shift right 
	ror _dtop+1
	ror _dtop
	jmp next

+header ~twomult, ~twomult_n, twodiv_n, 2, "2*"
	+code
	asl _dtop
	rol _dtop+1
	jmp next

;
; code lshift
; code rshift
;

+header ~lshift, ~lshift_n, twomult_n, 6, "LSHIFT"
	+code
	+dpop
	tax
	beq lshift_2
lshift_1:
	clc
	asl _dtop
	rol _dtop+1
	dex
	bne lshift_1
lshift_2:
	jmp next
			
+header ~rshift, ~rshift_n, lshift_n, 6, "RSHIFT"
	+code
	+dpop
	tax
	beq rshift_2
rshift_1:
	lsr _dtop+1
	ror _dtop
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

+header ~stod, ~stod_n, rshift_n, 3, "S>D"
	+forth
	+token dup, zerolt, qbranch
	+address stod_1
	+token minusone, exit
stod_1:
	+token zero, exit

; Optional Double=number word set
+header ~dnegate, ~dnegate_n, stod_n, 7, "DNEGATE"
	+forth
	+token invert, swap, invert, swap, one, mplus, exit 

; Optional Double-numbler word set
+header ~dabs, ~dabs_n, dnegate_n, 4, "DABS"
	+forth
	+token dup, zerolt, qbranch
	+address dabs_1
	+token dnegate
dabs_1:
	+token exit

;
; : fm/mod dup >r sm/rem
;          over dup 0<> swap 0< r@ 0< xor and
;          if 1- swap r> + swap else rdrop then ;
;

+header ~fmmod, ~fmmod_n, dabs_n, 6, "FM/MOD"
	+forth
	+token dup, tor, smrem, over, dup, zerone
	+token swap, zerolt, rat, zerolt, xor, and_op, qbranch
	+address fmmod_1
	+token oneminus, swap, rfrom, add, swap, branch
	+address fmmod_2
fmmod_1:
	+token rdrop
fmmod_2:
	+token exit

;
; : sm/rem 2dup xor >r ( Sign of the quotient) over >r ( Sign of the remainder)
;          abs >r dabs r> um/mod
;          swap r> 0< if negate then
;          swap r> 0< if negate then ;
;

+header ~smrem, ~smrem_n, fmmod_n, 6, "SM/REM"
	+forth
	+token twodup, xor, tor, over, tor
	+token abs, tor, dabs, rfrom, ummod
	+token swap, rfrom, zerolt, qbranch
	+address smrem_1
	+token negate
smrem_1:
	+token swap, rfrom, zerolt, qbranch
	+address smrem_2
	+token negate
smrem_2:
	+token exit

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

+header ~ummod, ~ummod_n, smrem_n, 6, "UM/MOD"
	+code
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

+header ~udmod, ~udmod_n, ummod_n, 6 + NST_FLAG, "UD/MOD"
	+forth
	+token tor, zero, rat, ummod
	+token rot, rot, rfrom, ummod, rot, exit

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

+header ~ummult, ~ummult_n, udmod_n, 3, "UM*"
	+code
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

+header ~mmult, ~mmult_n, ummult_n, 2, "M*"
	+forth
	+token twodup, xor, tor, abs, swap
	+token abs, ummult, rfrom, zerolt, qbranch
	+address mmult_1
	+token dnegate
mmult_1:
	+token exit

+header ~udmult, ~udmult_n, mmult_n, 3 + NST_FLAG, "UD*"
	+forth
	+token dup, tor, ummult, drop, swap
	+token rfrom, ummult, rot, add, exit

;
; : m+ s>d d+ ;
;

; From the optional Double-number word set
+header ~mplus, ~mplus_n, udmult_n, 2, "M+"
	+forth
	+token stod, dadd, exit

; ==============================================================================
; Logical operations. Note that all operations are performed bitwise

;
; code and
; code or
; code xor
; : invert -1 xor ;
;

+header ~and_op, ~and_n, mplus_n, 3, "AND"
	+code
	+dpop
	and _dtop
	sta _dtop
	txa
	and _dtop+1
	sta _dtop+1
	jmp next

+header ~or, ~or_n, and_n, 2, "OR"
	+code
	+dpop
	ora _dtop
	sta _dtop
	txa
	ora _dtop+1
	sta _dtop+1
	jmp next

+header ~xor, ~xor_n, or_n, 3, "XOR"
	+code
	+dpop
	eor _dtop
	sta _dtop
	txa
	eor _dtop+1
	sta _dtop+1
	jmp next

; Note that NOT has been removed from the standard.
+header ~invert, ~invert_n, xor_n, 6, "INVERT"
	+forth
	+token minusone, xor, exit

; Find lowest zero (free) bit index
+header ~freebit, ~freebit_n, invert_n, 7 + NST_FLAG, "FREEBIT"
	+code
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

+header ~setbit, ~setbit_n, freebit_n, 6 + NST_FLAG, "SETBIT"
	+forth
	+token dup, peek, rot, one, swap
	+token lshift, or, swap, poke, exit

+header ~clearbit, ~clearbit_n, setbit_n, 8 + NST_FLAG, "CLEARBIT"
	+forth
	+token dup, peek, rot, one, swap, lshift
	+token invert, and_op, swap, poke, exit

; ==============================================================================
; Comparisons

;
; code 0=
; code 0<
;

+header ~zeroeq, ~zeroeq_n, clearbit_n, 2, "0="
	+code
	ldx #255
	lda _dtop
	ora _dtop+1
	beq zeroeq_1
	ldx #0
zeroeq_1:
	stx _dtop
	stx _dtop+1
	jmp next

+header ~zerolt, ~zerolt_n, zeroeq_n, 2, "0<"
	+code
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

+header ~zerogt, ~zerogt_n, zerolt_n, 2, "0>"
	+forth
	+token zero, swap, less, exit
			
+header ~zerone, ~zerone_n, zerogt_n, 3, "0<>"
	+forth
	+token zeroeq, zeroeq, exit

+header ~equal, ~equal_n, zerone_n, 1, "="
	+forth
	+token sub, zeroeq, exit

+header ~notequal, ~notequal_n, equal_n, 2, "<>"
	+forth
	+token sub, zerone, exit


;
; code <
;
; Careful here. Some implementations have it as ": < - 0< ;" and it works... sometimes.
; Signed comparison on 6502 is surprisingly non-trivial. Refer to http://www.6502.org/tutorials/compare_beyond.html
; for details and tutorial
+header ~less, ~less_n, notequal_n, 1, "<"
	+code
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

+header ~greater, ~greater_n, less_n, 1, ">"
	+forth
	+token swap, less, exit

+header ~max, ~max_n, greater_n, 3, "MAX"
	+forth
	+token twodup, less, qbranch
	+address max_1
	+token swap
max_1:
	+token drop, exit

+header ~min, ~min_n, max_n, 3, "MIN"
	+forth
	+token twodup, greater, qbranch
	+address min_1
	+token swap
min_1:
	+token drop, exit

;
;	code u<
;
+header ~uless, ~uless_n, min_n, 2, "U<"
	+code
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

+header ~ugreater, ~ugreater_n, uless_n, 2, "U>"
	+forth
	+token swap, uless, exit

;
;	-1 constant true
;	0 constant false
;

+header ~true, ~true_n, ugreater_n, 4, "TRUE"
	+code doconst
	+value VAL_TRUE

+header ~false, ~false_n, true_n, 5, "FALSE"
	+code doconst
	+value VAL_FALSE

;
;	: within over - >r - r> u< ;
;

+header ~within, ~within_n, false_n, 6, "WITHIN"
	+forth
	+token over, sub, tor, sub, rfrom, uless, exit

; ==============================================================================
; Base stack operations.

;
;	code dup
;	code drop
;	code over
;	code swap
;

+header ~dup, ~dup_n, within_n, 3, "DUP"
	+code
	+ldax _dtop
	+dpush
	jmp next

+header ~drop, ~drop_n, dup_n, 4, "DROP"
	+code
	+dpop
	jmp next

+header ~over, ~over_n, drop_n, 4, "OVER"
	+code
	ldy #3
	lda (_dstack),y
	tax
	dey
	lda (_dstack),y
	+dpush
	jmp next

+header ~swap, ~swap_n, over_n, 4, "SWAP"
	+code
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

+header ~nip, ~nip_n, swap_n, 3, "NIP"
	+forth
	+token swap, drop, exit

+header ~tuck, ~tuck_n, nip_n, 4, "TUCK"
	+forth
	+token swap, over, exit

;
; : rot >r swap r> swap ;
;

+header ~rot, ~rot_n, tuck_n, 3, "ROT"
	+forth
	+token tor, swap, rfrom, swap, exit

;
;	code pick
;	code roll ; using reference implementation from forth-standard.org instead
;
; TODO - maybe check stack in both calls

+header ~pick, ~pick_n, rot_n, 4, "PICK"
	+code
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

+header ~roll, ~roll_n, pick_n, 4, "ROLL"
	+forth
	+token dup, qbranch
	+address roll_1 ; Using reference implementation
	+token swap, tor, oneminus, roll, rfrom, swap, exit
roll_1:
	+token drop, exit

;
;	code depth
;

+header ~depth, ~depth_n, roll_n, 5, "DEPTH"
	+code
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

+header ~twodrop, ~twodrop_n, depth_n, 5, "2DROP"
	+forth
	+token drop, drop, exit

+header ~twodup, ~twodup_n, twodrop_n, 4, "2DUP"
	+forth
	+token over, over, exit

+header ~twoswap, ~twoswap_n, twodup_n, 5, "2SWAP"
	+forth
	+token tor, rot, rot, rfrom, rot, rot, exit

+header ~twoover, ~twoover_n, twoswap_n, 5, "2OVER"
	+forth
	+token tor, tor, twodup, rfrom, rfrom, twoswap, exit

;
;	: ?dup dup if dup then ;
;

+header ~qdup, ~qdup_n, twoover_n, 4, "?DUP"
	+forth
	+token dup, qbranch
	+address qdup_1
	+token dup 
qdup_1:
	+token exit

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

+header ~cellplus, ~cellplus_n, qdup_n, 5, "CELL+"
	+forth
	+token twoplus, exit
			
+header ~cells, ~cells_n, cellplus_n, 5, "CELLS"
	+forth
	+token dup, add, exit
			
+header ~charplus, ~charplus_n, cells_n, 5, "CHAR+"
	+forth
	+token oneplus, exit
			
+header ~chars, ~chars_n, charplus_n, 5, "CHARS"
	+forth
	+token exit	; that's correct, just do nothing

+header ~align, ~align_n, chars_n, 5, "ALIGN"
	+forth
	+token exit

+header ~aligned, ~aligned_n, align_n, 7, "ALIGNED"
	+forth
	+token exit

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

+header ~rfrom, ~rfrom_n, aligned_n, 2, "R>"
	+code
	+rpop
	+dpush
	jmp next

+header ~tor, ~tor_n, rfrom_n, 2, ">R"
	+code
	+dpop
	+rpush
	jmp next

+header ~rat, ~rat_n, tor_n, 2, "R@"
	+code
	ldy #3
	lda (_rstack),y
	tax
	dey
	lda (_rstack),y
	+dpush
	jmp next

+header ~rdrop, ~rdrop_n, rat_n, 5 + NST_FLAG, "RDROP"
	+code
	+rpop
	jmp next

+header ~twotor, ~twotor_n, rdrop_n, 3, "2>R"
	+code
	+dpop
	+stax _rscratch
	+dpop
	+rpush
	+ldax _rscratch
	+rpush
	jmp next

+header ~tworfrom, ~tworfrom_n, twotor_n, 3, "2R>"
	+code
	+rpop
	+stax _rscratch
	+rpop
	+dpush
	+ldax _rscratch
	+dpush
	jmp next

+header ~tworat, ~tworat_n, tworfrom_n, 3, "2R@"
	+code
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

+header ~peek, ~peek_n, tworat_n, 1, "@"
	+code
	ldy #1
	lda (_dtop),y
	tax
	dey
	lda (_dtop),y
	+stax _dtop
	jmp next

+header ~cpeek, ~cpeek_n, peek_n, 2, "C@"
	+code
	ldy #0
	lda (_dtop),y
	sta _dtop
	sty _dtop+1
	jmp next

+header ~poke, ~poke_n, cpeek_n, 1, "!"
	+code
	+dpop
	+stax _wscratch
	+dpop
	ldy #0
	sta (_wscratch),y
	txa
	iny
	sta (_wscratch),y
	jmp next

+header ~cpoke, ~cpoke_n, poke_n, 2, "C!"
	+code
	+dpop
	+stax _wscratch
	+dpop
	ldy #0
	sta (_wscratch),y
	jmp next

+header ~twopoke, ~twopoke_n, cpoke_n, 2, "2!"
	+forth
	+token swap, over, poke, cellplus, poke, exit

+header ~twopeek, ~twopeek_n, twopoke_n, 2, "2@"
	+forth
	+token dup, cellplus, peek, swap, peek, exit

; ==============================================================================
; Literal. One of the most common words to see in the compiled code - will take the next parameter and
; push it to stack

;
; code lit
; Alternative but slower: : lit r@ peek r> cell+ >r ; nonstandard
;

+header ~lit, ~lit_n, twopeek_n, 3 + NST_FLAG, "LIT"
	+code
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

+header ~base, ~base_n, lit_n, 4, "BASE"
	+code doconst
	+value _base

+header ~bhash, ~bhash_n, base_n, 2, "<#"
	+forth
	+token lit
	+value _hldend
	+token lit
	+value _hld
	+token poke, exit

+header ~hash, ~hash_n, bhash_n, 1, "#"
	+forth
	+token base, peek, udmod, rot, lit
	+value '0'
	+token add
	+token dup, lit
	+value '9'
	+token greater, qbranch
	+address hash_1
	+token lit
	+value 7
	+token add
hash_1:
	+token hold, exit

+header ~hashs, ~hashs_n, hash_n, 2, "#S"
	+forth
hashs_1:
	+token hash, twodup, or, zeroeq, qbranch
	+address hashs_1
	+token exit

+header ~hashb, ~hashb_n, hashs_n, 2, "#>"
	+forth
	+token twodrop, lit
	+value _hld
	+token peek, lit
	+value _hldend
	+token over, sub, exit

+header ~hold, ~hold_n, hashb_n, 4, "HOLD"
	+forth
	+token lit
	+value _hld
	+token peek, one, sub, dup, lit
	+value _hld
	+token poke, cpoke, exit

+header ~holds, ~holds_n, hold_n, 5, "HOLDS"
	+forth
holds_1:
	+token dup, qbranch
	+address holds_2
	+token oneminus, twodup, add, cpeek, hold, branch
	+address holds_1
holds_2:
	+token twodrop, exit

+header ~sign, ~sign_n, holds_n, 4, "SIGN"
	+forth
	+token zerolt, qbranch
	+address sign_1
	+token lit
	+value '-'
	+token hold
sign_1:
	+token exit

;
; : d.r >r dup >r dabs <# #s r> sign #> r> over - spaces type ;
; : d. 0 d.r space ;
; : .r swap s>d rot d.r ;
; : u. 0 d.
; : u.r 0 swap d.r ;
; : . s>d d. ;
;

+header ~ddotr, ~ddotr_n, sign_n, 3, "D.R"
	+forth
	+token tor, dup, tor, dabs, bhash, hashs, rfrom
	+token sign, hashb, rfrom, over, sub, spaces, type, exit

+header ~ddot, ~ddot_n, ddotr_n, 2, "D."
	+forth
	+token zero, ddotr, space, exit

+header ~dotr, ~dotr_n, ddot_n, 2, ".R"
	+forth
	+token swap, stod, rot, ddotr, exit

+header ~udot, ~udot_n, dotr_n, 2, "U."
	+forth
	+token zero, ddot, exit

+header ~udotr, ~udotr_n, udot_n, 3, "U.R"
	+forth
	+token zero, swap, ddotr, exit

+header ~dot, ~dot_n, udotr_n, 1, "."
	+forth
	+token stod, ddot, exit

;
; : decimal 10 base ! ;
; : hex 16 base ! ;
;

+header ~decimal, ~decimal_n, dot_n, 7, "DECIMAL"
	+forth
	+token lit
	+value 10
	+token base, poke, exit

+header ~hex, ~hex_n, decimal_n, 3, "HEX"
	+forth
	+token lit
	+value 16
	+token base, poke, exit

; ==============================================================================
; HERE, comma, C,, etc.

;
; : +! dup @ rot + swap ! ;
; : here _here @ ;
; : allot _here +!
; : , here 2 allot ! ;
; : c, here 1 allot c! ;
;

+header ~incpoke, ~incpoke_n, hex_n, 2, "+!"
	+forth
	+token dup, peek, rot, add, swap, poke, exit

+header ~here, ~here_n, incpoke_n, 4, "HERE"
	+forth
	+token lit
	+value _here
	+token peek, exit

+header ~allot, ~allot_n, here_n, 5, "ALLOT"
	+forth
	+token lit
	+value _here
	+token incpoke, exit

+header ~comma, ~comma_n, allot_n, 1, ","
	+forth
	+token here, two, allot, poke, exit

+header ~ccomma, ~ccomma_n, comma_n, 2, "C,"
	+forth
	+token here, one, allot, cpoke, exit

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

+header ~branch, ~branch_n, ccomma_n, 6 + NST_FLAG, "BRANCH"
	+code
branch_c
	ldy #1
	lda (_ri),y
	tax
	dey
	lda (_ri),y
	+stax _ri
	jmp next

+header ~qbranch, ~qbranch_n, branch_n, 7 + NST_FLAG, "?BRANCH"
	+code
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

+header ~tib, ~tib_n, qbranch_n, 3 + NST_FLAG, "TIB"
	+forth
	+token lit
	+value _source
	+token peek, twoplus, twoplus, twoplus, peek, exit

+header ~ptrin, ~ptrin_n, tib_n, 3, ">IN"
	+forth
	+token lit
	+value _source
	+token peek, twoplus, twoplus, exit

+header ~numtib, ~numtib_n, ptrin_n, 4 + NST_FLAG, "#TIB"
	+forth
	+token lit
	+value _source
	+token peek, twoplus, twoplus, twoplus, twoplus, exit

+header ~source, ~source_n, numtib_n, 6, "SOURCE"
	+forth
	+token tib, numtib, peek, exit

+header ~sourceid, ~sourceid_n, source_n, 9, "SOURCE-ID"
	+forth
	+token lit
	+value _source
	+token peek, twoplus, peek, exit

+header ~accept, ~accept_n, sourceid_n, 6, "ACCEPT"
	+code
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

+header ~key, ~key_n, accept_n, 3, "KEY"
	+code
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
+header ~refill, ~refill_n, key_n, 6, "REFILL"
	+forth
	+token sourceid, zerolt, qbranch
	+address refill_1
	+token false, exit	; "EVALUATE" - no refill
refill_1:
	+token sourceid, zeroeq, qbranch
	+address refill_2
	+token cr, lit
	+value prompt
	+token count, type, cr, tib, lit
	+value 80
	+token accept, numtib, poke, zero, ptrin, poke, true, exit	; console
refill_2:
	+token sourceid, fileposition, drop, lit
	+value _source
	+token peek, lit
	+value 10
	+token add, twopoke
	+token tib, lit
	+value 98
	+token sourceid, readline, zeroeq, and_op, qbranch
	+address refill_3
	+token numtib, poke, zero, ptrin, poke, true, exit	 ; file (note that the position is saved _before_ the refill)
refill_3:
	+token drop, false, exit
prompt:
	+string 2, "OK"

;
; : save-input _source @ dup >r @ begin dup while dup 2* r@ + @ swap 1- again drop r> @ ;
;
; : restore-input over source-id = if
;                 source-id 0> if 6 pick 6 pick source-id reposition-file refill 2drop then
;                 begin dup while dup roll over 2* _source @ + ! 1- again drop false
;                 else true then ;
;

+header ~saveinput, ~saveinput_n, refill_n, 10, "SAVE-INPUT"
	+forth
	+token lit
	+value _source
	+token peek, dup, tor, peek
saveinput_1:
	+token dup, qbranch
	+address saveinput_2
	+token dup, twomult, rat, add, peek, swap, oneminus, branch
	+address saveinput_1
saveinput_2:
	+token drop, rfrom, peek, exit

+header ~restoreinput, ~restoreinput_n, saveinput_n, 13, "RESTORE-INPUT"
	+forth
	+token over, sourceid, equal, qbranch
	+address restoreinput_3
	+token sourceid, zerogt, qbranch
	+address restoreinput_1
	+token lit
	+value 6
	+token pick, lit
	+value 6
	+token pick, sourceid, repositionfile, refill, twodrop
restoreinput_1:
	+token dup, qbranch
	+address restoreinput_2
	+token dup, roll, over, twomult, lit
	+value _source
	+token peek, add, poke, oneminus, branch
	+address restoreinput_1
restoreinput_2:
	+token drop, false, exit
restoreinput_3:
	+token true, exit

; ==============================================================================
; Some basic text output

+header ~emit, ~emit_n, restoreinput_n, 4, "EMIT"
	+code
	+dpop
	jsr CHROUT
	jmp next

+header ~cr, ~cr_n, emit_n, 2, "CR"
	+forth
	+token lit
	+value NEW_LINE
	+token emit, exit

+header ~bl, ~bl_n, cr_n, 2, "BL"
	+code doconst
	+value ' '

+header ~space, ~space_n, bl_n, 5, "SPACE"
	+forth
	+token bl, emit, exit

+header ~type, ~type_n, space_n, 4, "TYPE"
	+forth
type_1:
	+token dup, zerone, qbranch
	+address type_done
	+token oneminus, swap, dup, cpeek, emit, oneplus, swap, branch
	+address type_1
type_done:
	+token drop, drop, exit

+header ~count, ~count_n, type_n, 5, "COUNT"
	+forth
	+token dup, oneplus, swap, cpeek, exit

; ==============================================================================
; Word lookup. This is where the complex things begin.

+header ~word, ~word_n, count_n, 4, "WORD"
	+forth
	+token tor, source, swap, ptrin, peek, add
word_1:
	+token over, ptrin, peek, greater, qbranch
	+address word_2
	+token dup, cpeek, lit
	+value 127 ; a workaround for peculiar C64 annoyance
	+token and_op, rat, equal, qbranch
	+address word_2
	+token ptrin, peek, oneplus, ptrin, poke, oneplus, branch
	+address word_1
word_2:
	+token twodrop, rfrom, parse, dup, lit
	+value _wordbuf
	+token cpoke
	+token lit
	+value _wordbuf
	+token oneplus, swap, cmove, lit
	+value _wordbuf
	+token exit

+header ~parse, ~parse_n, word_n, 5, "PARSE"
	+forth
	+token tor, source, ptrin, peek, sub
	+token oneplus, tor, ptrin, peek, add, dup, zero
parse_1:
	+token over, cpeek, rfrom, oneminus, dup, qbranch
	+address parse_3
	+token swap, lit
	+value 127
	+token and_op, rat, equal, qbranch
	+address parse_2 ; SAME AS ABOVE
	+token drop, swap, drop, rdrop, ptrin
	+token dup, peek, oneplus, swap, poke, exit
parse_2:
	+token tor, swap, oneplus, swap, oneplus, ptrin
	+token dup, peek, oneplus, swap, poke, branch
	+address parse_1
parse_3:
	+token twodrop, swap, drop, rdrop, exit

+header ~parsename, ~parsename_n, parse_n, 10, "PARSE-NAME"
	+forth
	+token source, swap, ptrin, peek, add
parsename_1:
	+token over, ptrin, peek, greater, qbranch
	+address parsename_2
	+token dup, cpeek, bl, equal, qbranch
	+address parsename_2
	+token ptrin, peek, oneplus, ptrin, poke, oneplus, branch
	+address parsename_1
parsename_2:
	+token twodrop, bl, parse, exit

; Forth systems typically have a few words to move between different parts of a vocabulary word. In the direct
; threaded code the only non-trivial move is the one between LFA and NFA. In this particular model it abuses the
; fact that the maximum NFA length is 32+1 and the name cannot include characters with codes below 32. 
+header ~lfatonfa, ~lfatonfa_n, parsename_n, 6 + NST_FLAG, "L>NAME"
	+forth
	+token oneminus, zero
lfatonfa_1:
	+token over, cpeek, lit
	+value NAMEMASK
	+token and_op, over, notequal, qbranch
	+address lfatonfa_2
	+token swap, oneminus, swap, oneplus, dup, lit
	+value 32
	+token equal, qbranch
	+address lfatonfa_1
	+token drop, drop, zero, exit
lfatonfa_2:
	+token drop, exit

+header ~tobody, ~tobody_n, lfatonfa_n, 5, ">BODY"
	+forth
	+token oneplus, twoplus, exit

+header ~context, ~context_n, tobody_n, 7 + NST_FLAG, "CONTEXT"
	+code doconst
	+value _context

+header ~latest, ~latest_n, context_n, 6 + NST_FLAG, "LATEST"
	+forth
	+token lit
	+value _latest
	+token peek, exit


cstr1 = _dtop
clen1 = _scratch
cstr2 = _rscratch
clen2 = _wscratch

; COMPARE became standard in the later versions of the language.
; In optional String word set
; This one still can be optimized further
+header ~compare, ~compare_n, latest_n, 7, "COMPARE" ; (caddr1, u1, caddr2, u2 -> n)
	+code
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


+header ~strict, ~strict_n, compare_n, 6 + NST_FLAG, "STRICT"
	+forth
	+token true, lit
	+value _strict
	+token poke, exit

; The only non-standard word that is visible in "strict" mode
+header ~extended, ~extended_n, strict_n, 8, "EXTENDED"
	+forth
	+token false, lit
	+value _strict
	+token poke, exit

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

+header ~xfind, ~xfind_n, extended_n, 6 + NST_FLAG, "(FIND)"		; caddr, n -> NFA | 0
	+code
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


+header ~find, ~find_n, xfind_n, 4, "FIND"
	+forth
	+token dup, tor, count, xfind, dup, qbranch
	+address find_1
	+token rdrop, dup, count, lit
	+value NAMEMASK
	+token and_op, add, twoplus
	+token swap, cpeek, lit
	+value IMM_FLAG
	+token and_op, minusone, swap, qbranch
	+address find_2
	+token negate, exit
find_1:
	+token rfrom, swap
find_2:
	+token exit

+header ~immediate, ~immediate_n, find_n, 9, "IMMEDIATE"
	+forth
	+token latest, dup, cpeek, lit
	+value IMM_FLAG
	+token or, swap, cpoke, exit

+header ~nonstandard, ~nonstandard_n, immediate_n, 11 + NST_FLAG, "NONSTANDARD"
	+forth
	+token latest, dup, cpeek, lit
	+value IMM_FLAG
	+token or, swap, cpoke, exit

+header ~xdigit, ~xdigit_n, nonstandard_n, 7 + NST_FLAG, "(DIGIT)"
	+forth
	+token dup, lit
	+value 48
	+token less, qbranch
	+address xdigit_1
xdigit_x:
	+token drop, minusone, exit
xdigit_1:
	+token dup, lit
	+value 58
	+token less, qbranch
	+address xdigit_2
	+token lit
	+value 48
	+token sub, exit ; '0'-'9'
xdigit_2:
	+token dup, lit
	+value 65
	+token less, qbranch
	+address xdigit_3
	+token branch
	+address xdigit_x
xdigit_3:
	+token dup, lit
	+value 91
	+token less, qbranch
	+address xdigit_4
	+token lit
	+value 55
	+token sub, exit ; 'A'-'Z'
xdigit_4:
	+token dup, lit
	+value 97
	+token less, qbranch
	+address xdigit_5
	+token branch
	+address xdigit_x
xdigit_5:
	+token dup, lit
	+value 123
	+token less, qbranch
	+address xdigit_x
	+token lit
	+value 87
	+token sub, exit ; 'a'-'z'

+header ~tonumber, ~tonumber_n, xdigit_n, 7, ">NUMBER"
	+forth
tonumber_1:
	+token dup, zerogt, qbranch
	+address tonumber_3									; no more digits left?
	+token over, cpeek, xdigit, dup, zerolt, zeroeq, qbranch
	+address tonumber_2	; not a possible digit?
	+token dup, base, peek, less, qbranch
	+address tonumber_2						; not a digit in current base?
	+token swap, oneminus, tor, swap, oneplus, tor, tor
	+token base, peek, udmult, rfrom, mplus, rfrom, rfrom
	+token branch
	+address tonumber_1												; and repeat
tonumber_2:
	+token drop
tonumber_3:
	+token exit

+header ~number, ~number_n, tonumber_n, 6 + NST_FLAG, "NUMBER"
	+forth
	+token count, base, peek, tor
	+token dup, lit
	+value 3
	+token equal, two, pick, cpeek, lit
	+value 39
	+token equal, and_op			; character as 'c'
	+token two, pick, two, add, cpeek, lit
	+value 39
	+token equal, and_op, qbranch
	+address number_8
	+token drop, oneplus, cpeek, branch
	+address number_5
number_8:
	+token dup, one, greater, qbranch
	+address number_9
	+token over, cpeek, lit
	+value 35
	+token equal, qbranch
	+address number_11
	+token decimal, branch
	+address number_10
number_11:
	+token over, cpeek, lit
	+value 36
	+token equal, qbranch
	+address number_12
	+token hex, branch
	+address number_10
number_12:
	+token over, cpeek, lit
	+value 37
	+token equal, qbranch
	+address number_9
	+token two, base, poke
number_10:
	+token swap, oneplus, swap, oneminus
number_9:
	+token twodup, false, tor, over, cpeek, lit
	+value 45
	+token equal, qbranch
	+address number_1
	+token rdrop, true, tor, oneminus, swap, oneplus, swap
number_1:
	+token zero, dup, twoswap, tonumber, dup, qbranch
	+address number_4
	+token one, equal, swap, cpeek, lit
	+value 46
	+token equal, and_op, qbranch
	+address number_7	; one unconverted char and it's '.'?
	+token rfrom, qbranch
	+address number_2
	+token dnegate
number_2:
	+token twoswap, twodrop, state, peek, qbranch
	+address number_3
	+token compile, lit, swap
	+token comma, compile, lit, comma
number_3:
	+token branch
	+address number_6
number_4:
	+token twodrop, twoswap, twodrop, drop, rfrom, qbranch
	+address number_5
	+token negate
number_5:
	+token state, peek, qbranch
	+address number_6
	+token compile, lit, comma
number_6:
	+token rfrom, base, poke, exit
number_7:
	+token twodrop, type, xabortq
	+string 2, " ?"


; ==============================================================================
; Nice service word that prints the entire list of supported words.

; In optional Programming-Tools word set
+header ~words, ~words_n, number_n, 5, "WORDS"
	+forth
	+token zero, context, peek
words_1:
	+token dup, count, dup, lit
	+value NST_FLAG
	+token and_op, lit
	+value _strict
	+token peek, and_op, swap
	+token lit
	+value NAMEMASK
	+token and_op, dup, tor, swap, zeroeq, qbranch
	+address words_2
	+token dup, zerone, qbranch
	+address words_2
	+token type, bl, emit, swap, oneplus, swap, branch
	+address words_3
words_2:
	+token twodrop
words_3:
	+token rfrom, oneplus, add, peek, dup, zeroeq, qbranch
	+address words_1
	+token drop, cr, dot, lit
	+value words_n
	+token count, type, exit

; ==============================================================================
; Outer interpreter

+header ~state, ~state_n, words_n, 5, "STATE"
	+code doconst
	+value _state

+header ~qcomp, ~qcomp_n, state_n,  5 + NST_FLAG, "?COMP"
	+forth
	+token state, peek, zeroeq, qbranch
	+address qcomp_2
	+token rat, twominus, twominus, dup, peek, lit
	+value call
	+token equal, qbranch
	+address qcomp_1	; if ?COMP immediately follows CALL
	+token oneminus, twominus, lfatonfa, count, lit
	+value NAMEMASK
	+token and_op, type, space					; output the word name with error
qcomp_1:
	+token xabortq
	+string 20, "REQUIRES COMPILATION"
qcomp_2:
	+token exit

+header ~qstack, ~qstack_n, qcomp_n, 6 + NST_FLAG, "?STACK"
	+forth
	+token depth, dup, zerolt, swap, lit
	+value STACKLIMIT
	+token greater, or, qbranch
	+address qstack_1
	+token xabortq
	+string 11, "STACK FAULT"
qstack_1:
	+token exit

+header ~interpret, ~interpret_n, qstack_n, 9 + NST_FLAG, "INTERPRET"
	+forth
interpret_1:
	+token qstack, bl, word, dup, cpeek, qbranch
	+address interpret_done	; get the next word if any
	+token state, peek, qbranch
	+address interpret_int
	+token find, dup, qbranch
	+address comp_num
	+token zerolt, qbranch
	+address comp_imm		; compiling now
	+token comma, branch
	+address interpret_1		; regular word in compile mode
comp_imm:
	+token execute, branch
	+address interpret_1		; immediate word in compile mode
comp_num:
	+token drop, number, branch
	+address interpret_1
interpret_int:
	+token find, qbranch
	+address int_num			; interpreting now
	+token execute, branch
	+address interpret_1		; any word in interpreter mode
int_num:
	+token number, branch
	+address interpret_1
interpret_done:
	+token drop, refill, zeroeq, qbranch
	+address interpret_1
	+token closesource, exit

+header ~closesource, ~closesource_n, interpret_n, 12 + NST_FLAG, "CLOSE-SOURCE"
	+forth
	+token sourceid, zerone, qbranch
	+address closesource_2						; nothing to do with console source
	+token sourceid, zerogt, qbranch
	+address closesource_1
	+token sourceid, closefile, drop, minusone, lit
	+value _ibufcount
	+token incpoke		; close file and release the buffer
closesource_1:
	+token lit
	+value _source
	+token dup, peek, dup, peek, oneplus
	+token cells, add, swap, poke	; this will close the last frame
closesource_2:
	+token exit

+header ~evaluate, ~evaluate_n, closesource_n, 8, "EVALUATE"
	+forth
	+token lit
	+value _source
	+token peek
	+token twominus, swap, over, poke
	+token twominus, swap, over, poke
	+token twominus, zero, over, poke
	+token twominus, minusone, over, poke
	+token twominus, lit
	+value 4
	+token over, poke
	+token lit
	+value _source
	+token poke
	+token state, qbranch
	+address evaluate_1
	+token interpret
evaluate_1:
	+token exit

; ==============================================================================
; Colon definition and related words

+header ~xcreate, ~xcreate_n, evaluate_n, 8 + NST_FLAG, "(CREATE)"
	+forth
	+token here, tor
	+token dup, lit
	+value 32
	+token greater, qbranch
	+address xcreate_1
	+token drop, lit
	+value 31 ; limit the word to 31 characters
xcreate_1:
	+token dup, ccomma, rat, oneplus, swap, dup, allot, cmove	; set NFA
	+token latest, comma										; set LFA
	+token rfrom, lit
	+value _latest
	+token poke
	+token lit
	+value JMP_INSTR	; for direct threading!
	+token ccomma
	+token lit
	+value created
	+token comma									; set CFA to a special code that pushes PFA on the stack
	+token latest, context, poke, exit							; and add to the search order

+header ~create, ~create_n, xcreate_n, 6, "CREATE"
	+forth
	+token bl, word, count, xcreate, exit

; DOES> is a weird beast. It generates code that will modify the execution of the
; last defined word to jump to the definition word. It is also quite non-portable as it generates a low level instruction
+header ~xcode, ~xcode_n, create_n, 7 + NST_FLAG, "(;CODE)"
	+forth
	+token rfrom										; which is the address of the "call xdoes" instruction
	+token latest, count, lit
	+value NAMEMASK
	+token and_op, add, twoplus	; CFA of the last defined word
	+token oneplus ; PFA (!)
	+token poke, exit										; and this will actually exit the defining word

; Note that while DOES> looks like high-level word its implementation is depended on the opcode for native CALL/JSR
+header ~doesx, ~doesx_n, xcode_n, 5 + IMM_FLAG, "DOES>"
	+forth
	+token qcomp, compile, xcode, lit
	+value JSR_INSTR
	+token ccomma, lit
	+value does
	+token comma, exit	; compile (;CODE) followed by "call does_c"

; Note that colon will remove the word from the search order (to be restored by semicolon)
+header ~colon, ~colon_n, doesx_n, 1, ":"
	+forth
	+token create
	+token lit
	+value call
	+token here, twominus, dup, tor, poke, rfrom
	+token oneminus, twominus, peek, context, poke, bracketx, exit

; Words defined with :NONAME technically don't need to be linked in the vocabulary but if it is done that way RECURSE becomes harder
; to implement. It is easier just to link the word with emtpy name. In this implementation it has an unusual side effect that FIND
; will actually find the last :NONAME if searched for empty string and the test suite actually traps that (not an error though). But -
; standard does not specify it either way; and this is potentially useful.

;
; : :noname here 0 , latest , _latest ! here ' call , ] ;
;
 
+header ~colonnoname, ~colonnoname_n, colon_n, 7, ":NONAME"
	+forth
	+token here, zero, ccomma			; set 0-length NFA
	+token latest, comma, lit
	+value _latest
	+token poke		; LFA
	+token here
	+token lit
	+value JMP_INSTR
	+token ccomma
	+token lit
	+value call
	+token comma, bracketx, exit	; CFA and keep the address on stack

+header ~bufferc, ~bufferc_n, colonnoname_n, 7, "BUFFER:"
	+forth
	+token create, allot, exit
				
+header ~semicolon, ~semicolon_n, bufferc_n, 1 + IMM_FLAG, ";"
	+forth
	+token qcomp, compile, exit, bracket, latest, context, poke, exit

+header ~variable, ~variable_n, semicolon_n, 8, "VARIABLE"
	+forth
	+token create, zero, comma, exit

+header ~twovariable, ~twovariable_n, variable_n, 9, "2VARIABLE"
	+forth
	+token create, zero, dup, comma, comma, exit

+header ~constant, ~constant_n, twovariable_n, 8, "CONSTANT"
	+forth
	+token create, comma, xcode
	!byte JSR_INSTR
	+token does, peek, exit

+header ~defer, ~defer_n, constant_n, 5, "DEFER"
	+forth
	+token create, lit
	+value dodefer
	+token here, twominus, poke, compile, exit, exit

+header ~qdefer, ~qdefer_n, defer_n, 6 + NST_FLAG, "?DEFER"
	+forth
	+token dup, oneplus, peek, lit
	+value dodefer
	+token equal, qbranch
	+address qdefer_1
	+token exit
qdefer_1:
	+token xabortq
	+string 12, "NOT DEFER'ED"

+header ~deferpeek, ~deferpeek_n, qdefer_n, 6, "DEFER@"
	+forth
	+token qdefer, tobody, peek, exit

+header ~deferpoke, ~deferpoke_n, deferpeek_n, 6, "DEFER!"
	+forth
	+token qdefer, tobody, poke, exit

+header ~actionof, ~actionof_n, deferpoke_n, 9 + IMM_FLAG, "ACTION-OF"
	+forth
	+token state, peek, qbranch
	+address actionof_1
	+token btick, compile, deferpeek, exit
actionof_1:
	+token tick, deferpeek, exit

+header ~is, ~is_n, actionof_n, 2 + IMM_FLAG, "IS"
	+forth
	+token state, peek, qbranch
	+address is_1
	+token btick, compile, deferpoke, exit
is_1:
	+token tick, deferpoke, exit

; "value" has a special structure: three tokens for read semantics,
; write semantics, and compile semantics, followed by the value itself

+header ~value, ~value_n, is_n, 5, "VALUE"
	+forth
	+token create, lit
	+value dovalue
	+token here, twominus, poke, lit
	+value value_sem
	+token comma, comma, exit
value_sem:
	+token peek, poke, comppoke

+header_internal ~comppoke
	+forth
	+token compile, lit, comma, compile, poke, exit

+header ~to, ~to_n, value_n, 2 + IMM_FLAG, "TO"
	+forth
	+token bl, word, find, drop, oneplus, dup, peek, lit
	+value dovalue
	+token equal, qbranch
	+address to_2
	+token twoplus, dup, twoplus, swap, peek, state, peek, qbranch
	+address to_1
	+token twoplus
to_1:
	+token twoplus, peek, execute, exit
to_2:
	+token xabortq
	+string 11, "NOT A VALUE"

+header ~tick, ~tick_n, to_n, 1, "'"
	+forth
	+token bl, word, find, drop, exit

+header ~btick, ~btick_n, tick_n, 3 + IMM_FLAG, "[']"
	+forth
	+token qcomp, bl, word, find, drop, compile, lit, comma
	+token exit

; This will get the next parameter, compile it to the current definition and skip
+header ~compile, ~compile_n, btick_n, 7 + NST_FLAG, "COMPILE"
	+forth
	+token rfrom, dup, twoplus, tor, peek, comma, exit

+header ~bcompile, ~bcompile_n, compile_n, 9 + IMM_FLAG, "[COMPILE]"
	+forth
	+token qcomp, tick, comma, exit

; Somehow I've managed to get this to pass the tests but I still don't completely understand what
; it is supposed to do
+header ~postpone, ~postpone_n, bcompile_n, 8 + IMM_FLAG, "POSTPONE"
	+forth
	+token qcomp, bl, word, find, one, equal, qbranch
	+address postpone_1
	+token comma, exit
postpone_1:
	+token lit
	+value compile
	+token comma, comma, exit

; This word behaves differently depending on compilation state - in compilation it
; will emit LIT followed by the value from the stack
+header ~literal, ~literal_n, postpone_n, 7 + IMM_FLAG, "LITERAL"
	+forth
	+token qcomp, state, peek, qbranch
	+address literal_1
	+token compile, lit, comma
literal_1:
	+token exit

+header ~bracket, ~bracket_n, literal_n, 1 + IMM_FLAG, "["
	+forth
	+token qcomp, false, state, poke, exit

+header ~bracketx, ~bracketx_n, bracket_n, 1, "]"
	+forth
	+token true, state, poke, exit

;
; : ( source-id 0< if
;     begin ')' parse 2drop >in @ #tib @ = tib #tib @ + 1- c@ ')' = and
;     while refill invert if exit then again
;     else ')' parse 2drop then ; immediate
;

+header ~brace, ~brace_n, bracketx_n, 1 + IMM_FLAG, "("
	+forth
	+token sourceid, zerogt, qbranch
	+address brace_2
brace_1:
	+token lit
	+value ')'
	+token parse, twodrop, ptrin, peek, numtib, peek, equal
	+token tib, numtib, peek, add, oneminus, cpeek, lit
	+value ')'
	+token notequal, and_op
	+token qbranch
	+address brace_3
	+token refill, invert, qbranch
	+address brace_1
	+token exit
brace_2:
	+token lit
	+value ')'
	+token parse, twodrop
brace_3:
	+token exit

+header ~backslash, ~backslash_n, brace_n, 1 + IMM_FLAG, "\\"
	+forth
	+token zero, parse, twodrop, exit

+header ~dotbrace, ~dotbrace_n, backslash_n, 2 + IMM_FLAG, ".("
	+forth
	+token lit
	+value ')'
	+token parse, type, exit

+header ~commaquote, ~commaquote_n, dotbrace_n, 2 + NST_FLAG, ",\""
	+forth
	+token lit
	+value '"'
	+token parse, dup, ccomma, here
	+token over, allot, swap, cmove, exit

+header ~cquote, ~cquote_n, commaquote_n, 2 + IMM_FLAG, "C\""
	+forth
	+token qcomp, compile, branch, fmark
	+token here, swap, commaquote, fresolve, compile, lit, comma
	+token exit

+header ~squote, ~squote_n, cquote_n, 2 + IMM_FLAG, "S\""
	+forth
	+token state, peek, zerone, qbranch
	+address squote_1
	+token cquote, compile, count, exit
squote_1:
	+token lit
	+value '"'
	+token parse, lit
	+value _sbuf
	+token lit
	+value _sflip
	+token peek, lit
	+value 100
	+token mult, add, swap, twotor, tworat, cmove
	+token tworfrom, lit
	+value _sflip
	+token dup, peek, one, xor, swap, poke, exit 

+header ~ssquote, ~ssquote_n, squote_n, 3 + IMM_FLAG, "S\\\""
	+forth
	+token tib, ptrin, peek, add, lit
	+value _sbuf
	+token lit
	+value _sflip
	+token peek, lit
	+value 100
	+token mult, add, numtib, peek
	+token ptrin, peek, sub, over, tor
	+token smove, swap, ptrin, incpoke
	+token rfrom, swap, lit
	+value _sflip
	+token dup, peek, one, xor, swap, poke
	+token state, peek, qbranch
	+address ssquote_1
	+token compile, branch, fmark
	+token here, two, pick, dup, ccomma, allot, swap, fresolve
	+token compile, lit, dup
	+token comma, compile, count, oneplus, swap, cmove 
ssquote_1:
	+token exit

+header ~dotquote, ~dotquote_n, ssquote_n, 2 + IMM_FLAG, ".\""
	+forth
	+token qcomp, cquote, compile, count, compile, type, exit

+header ~compilecomma, ~compilecomma_n, dotquote_n, 8, "COMPILE,"
	+forth
	+token comma, exit

+header ~char, ~char_n, compilecomma_n, 4, "CHAR"
	+forth
	+token bl, word, charplus, cpeek, exit

+header ~bcharb, ~bcharb_n, char_n, 6 + IMM_FLAG, "[CHAR]"
	+forth
	+token compile, lit, bl
	+token word, charplus, cpeek, comma, exit

+header ~xabortq, ~xabortq_n, bcharb_n, 8 + NST_FLAG, "(ABORT\")"
	+forth
	+token rat, count, type, abort, exit

+header ~abortq, ~abortq_n, xabortq_n, 6 + IMM_FLAG, "ABORT\""
	+forth
	+token qcomp, compile, qbranch, fmark
	+token compile, xabortq, commaquote, fresolve, exit

; In optional Programming-Tools word set
+header ~forget, ~forget_n, abortq_n, 6, "FORGET"
	+forth
	+token bl, word, find, zerone, qbranch
	+address forget_1
	+token twominus, dup, peek, dup, lit
	+value _latest
	+token poke, context, poke
	+token lfatonfa, lit
	+value _here
	+token poke, exit
forget_1:
	+token drop, exit

+header ~marker, ~marker_n, forget_n, 6, "MARKER"
	+forth
	+token create, xcode
	!byte JSR_INSTR
	+token does
	+token oneminus, twominus, twominus, dup, peek, dup, lit
	+value _latest
	+token poke, context, poke
	+token lfatonfa, lit
	+value _here
	+token poke, exit

+header ~recurse, ~recurse_n, marker_n, 7 + IMM_FLAG, "RECURSE"
	+forth
	+token qcomp, latest, count, add, cellplus, comma, exit

; ==============================================================================
; More control structure support

+header ~fmark, ~fmark_n, recurse_n, 5 + NST_FLAG, ">MARK"
	+forth
	+token here, zero, comma, exit

+header ~fresolve, ~fresolve_n, fmark_n, 8 + NST_FLAG, ">RESOLVE"
	+forth
	+token here, swap, poke, exit

+header ~rmark, ~rmark_n, fresolve_n, 5 + NST_FLAG, "<MARK"
	+forth
	+token here, exit

+header ~rresolve, ~rresolve_n, rmark_n, 8 + NST_FLAG, "<RESOLVE"
	+forth
	+token comma, exit

; ==============================================================================
; Control words. All of these are immediate and make don't do anything useful
; in interpreter mode. There should be no code calling to CFA of these words,
; the CFA labels are removed to enforce that
; To understand the concept behind these words look at the BEGIN/AGAIN pair -
; BEGIN ends up just putting RI on the stack and AGAIN compiles BRANCH to that RI.
; Forward references are a bit trickier but follow the same pattern.

+header ~begin, ~begin_n, rresolve_n, 5 + IMM_FLAG, "BEGIN"
	+forth
	+token qcomp, rmark, exit

+header ~until, ~until_n, begin_n, 5 + IMM_FLAG, "UNTIL"
	+forth
	+token qcomp, compile, qbranch, rresolve
	+token exit

+header ~again, ~again_n, until_n, 5 + IMM_FLAG, "AGAIN"
	+forth
	+token qcomp, compile, branch, rresolve
	+token exit

+header ~if, ~if_n, again_n, 2 + IMM_FLAG, "IF"
	+forth
	+token qcomp, compile, qbranch, fmark
	+token exit

+header ~then, ~then_n, if_n, 4 + IMM_FLAG, "THEN"
	+forth
	+token qcomp, fresolve, exit

+header ~else, ~else_n, then_n, 4 + IMM_FLAG, "ELSE"
	+forth
	+token qcomp, compile, branch, fmark
	+token swap, fresolve, exit

+header ~while, ~while_n, else_n, 5 + IMM_FLAG, "WHILE"
	+forth
	+token qcomp, compile, qbranch, fmark
	+token swap, exit

+header ~repeat, ~repeat_n, while_n, 6 + IMM_FLAG, "REPEAT"
	+forth
	+token qcomp, compile, branch, rresolve
	+token fresolve, exit

; DO/LOOP are considerably more complex so they are often implemented with helper words. In practical
; implementations these helpers are done in native code, so are I, J, and LEAVE.
; (DO) stores on the return stack: leaveaddr, limit, current, (ret) 
+header ~xdo, ~xdo_n, repeat_n, 4 + NST_FLAG, "(DO)"
	+forth
	+token rfrom, dup, peek, tor	; forward ref for LEAVE
	+token rot, tor, swap, tor
	+token twoplus, tor				; step over the actual forward ref
	+token exit

+header ~xqdo, ~xqdo_n, xdo_n, 5 + NST_FLAG, "(?DO)"
	+forth
	+token twodup, equal, qbranch
	+address xqdo_1
	+token twodrop, rfrom, peek, tor, exit
xqdo_1:
	+token rfrom, dup, peek, tor	; forward ref for LEAVE
	+token rot, tor, swap, tor
	+token twoplus, tor				; step over the actual forward ref
	+token exit
			
; and (LOOP) adjusts the values on rstack or just drops the top three values from it to exit
+header ~xloop, ~xloop_n, xqdo_n, 6 + NST_FLAG, "(LOOP)"
	+forth
	+token rfrom				; return address is only needed to get the backref
	+token rfrom, oneplus			; new value of current
	+token rat, over, equal, qbranch
	+address xloop_1
	+token drop, drop, rdrop, exit	; exit the loop (leaveaddr on the rstack)
xloop_1:
	+token tor, peek, tor, exit		; continue the loop

+header ~xploop, ~xploop_n, xloop_n, 7 + NST_FLAG, "(+LOOP)"
	+forth
	+token rfrom, swap		; return address is only needed to get the backref / addr, step
	+token dup, rat, add			; preserve step value and get new value of current / addr, step, newcur
	+token rfrom, rat, sub			; diff limit and new current / addr, step, newcur, olddiff
	+token rot, over, xor, zerolt, swap ; new diff and step have different signs? / addr, newcur, step^olddiff<0, olddiff
	+token two, pick, rat, sub		; diff limit and previous current / addr, newcur, s^d, olddiff, newdiff
	+token xor, zerolt, and_op, qbranch
	+address xploop_1  ; or diffs before and after have different signs / newdiff^olddiff < 0
	+token drop, drop, rdrop, exit	; exit the loop (leaveaddr on the rstack)
xploop_1:
	+token tor, peek, tor, exit		; continue the loop

+header ~do, ~do_n, xploop_n, 2 + IMM_FLAG, "DO"
	+forth
	+token qcomp, compile, xdo, fmark, rmark, exit

+header ~qdo, ~qdo_n, do_n, 3 + IMM_FLAG, "?DO"
	+forth
	+token qcomp, compile, xqdo, fmark, rmark, exit

+header ~loop, ~loop_n, qdo_n, 4 + IMM_FLAG, "LOOP"
	+forth
	+token qcomp, compile, xloop, rresolve, fresolve, exit

+header ~ploop, ~ploop_n, loop_n, 5 + IMM_FLAG, "+LOOP"
	+forth
	+token qcomp, compile, xploop, rresolve, fresolve, exit

+header ~i, ~i_n, ploop_n, 1, "I"
	+forth
	+token rfrom, rat, swap, tor, exit

+header ~j, ~j_n, i_n, 1, "J"
	+forth
	+token rfrom, rfrom, rfrom, rfrom, rfrom, dup, tor
	+token swap, tor, swap, tor, swap, tor, swap, tor
	+token exit

+header ~leave, ~leave_n, j_n, 5, "LEAVE"
	+forth
	+token rdrop, rdrop, rdrop, exit
			
+header ~unloop, ~unloop_n, leave_n, 6, "UNLOOP"
	+forth
	+token rfrom, rdrop, rdrop, rdrop, tor, exit

+header ~case, ~case_n, unloop_n, 4 + IMM_FLAG, "CASE"
	+forth
	+token qcomp, depth, rfrom, swap, tor, tor, exit

+header ~of, ~of_n, case_n, 2 + IMM_FLAG, "OF"
	+forth
	+token qcomp, compile, over, compile, equal, compile, qbranch
	+token fmark, compile, drop, exit

+header ~endof, ~endof_n, of_n, 5 + IMM_FLAG, "ENDOF"
	+forth
	+token qcomp, compile, branch, fmark
	+token swap, fresolve, exit

+header ~endcase, ~endcase_n, endof_n, 7 + IMM_FLAG, "ENDCASE"
	+forth
	+token qcomp, compile, drop, depth
	+token rfrom, rfrom, swap, tor, sub
endcase_1:
	+token dup, qbranch
	+address endcase_2
	+token oneminus, swap, fresolve, branch
	+address endcase_1
endcase_2:
	+token drop, exit

; ==============================================================================
; Some nice to have words

+header ~spaces, ~spaces_n, endcase_n, 6, "SPACES"
	+forth
spaces_1:
	+token dup, zerogt, qbranch
	+address spaces_2
	+token oneminus, space, branch
	+address spaces_1
spaces_2:
	+token drop, exit

; In optional String word set
+header ~cmove, ~cmove_n, spaces_n, 5, "CMOVE"
	+code
	+dpop
	+stax _scratch
	+dpop
	+stax _wscratch
	+dpop
	+stax _rscratch
	
	jsr movedown
	jmp next

; In optional String word set
+header ~cmovex, ~cmovex_n, cmove_n, 6, "CMOVE>"
	+code
	+dpop
	+stax _scratch
	+dpop
	+stax _wscratch
	+dpop
	+stax _rscratch
	
	jsr moveup
	jmp next

+header ~move, ~move_n, cmovex_n, 4, "MOVE"
	+forth
	+token rot, rot, twodup, less, qbranch
	+address move_1
	+token rot, cmovex, exit
move_1:
	+token rot, cmove, exit

; Non-standard word, similar to CMOVE but does character conversions for S\". Returns number
; of characters processed and returned
; addr_from, addr_to, len_limit -> len_actual, let_result
; Note that this implementation is an overkill for S\" word - the string in that word cannot
; possibly be longer than 100 characters.
_sactual = _dtop
_sresult = _scratch_1
_stemp = _scratch_2

+header ~smove, ~smove_n, move_n, 5 + NST_FLAG, "SMOVE"
	+code
	+dpop
	+stax _scratch
	+dpop
	+stax _wscratch
	+ldax _dtop
	+stax _rscratch
	
	lda #0
	sta _sactual
	sta _sactual+1
	sta _sresult
	sta _sresult+1

	tay
	ldx _scratch+1
	beq smove_2
smove_1:
	jsr smove_char
	iny
	bne smove_1
	inc _rscratch+1
	inc _wscratch+1
	dex
	bne smove_1
smove_2:
	ldx _scratch
	beq smove_4
smove_3:
	jsr smove_char
	iny
	dex
	bne smove_3
smove_4:

	+ldax _sresult
	+dpush
	jmp next

smove_char:
	+inc16 _sactual		; increasing the actual count before any checks so it will include the quote
	lda (_rscratch),y
	cmp #'\"'
	beq smove_7			; end of the string
	cmp #'\\'
	bne smove_8			; is this an escaped character
	+inc16 _sactual
	+inc16 _rscratch
	lda (_rscratch),y
	cmp #'M'
	bne smove_9			; 'm' translated into two character
	lda #13
	sta (_wscratch),y
	+inc16 _wscratch
	+inc16 _sresult
	lda #10
	bne smove_8
smove_9:
	cmp #'X'			; 'x' is a hex sequence
	bne smove_10
	+inc16 _sactual
	+inc16 _rscratch
	lda (_rscratch),y
	jsr smove_hexdigit
	asl
	asl
	asl
	asl
	sta _stemp
	+inc16 _sactual
	+inc16 _rscratch
	lda (_rscratch),y
	jsr smove_hexdigit
	ora _stemp
	jmp smove_8
smove_10:
	cmp #'A'
	bmi smove_8
	cmp #'Z'+1
	bpl smove_8
	stx _stemp
	+sub 'A'
	tax
	lda smove_subst,x
	ldx _stemp
smove_8:
	sta (_wscratch),y
	+inc16 _sresult		; increasing the result after the character is written to the destination
	rts
smove_7:
	ldx #0
	stx _scratch
	inx					; this will instantly terminate both loops in the caller
	rts

smove_hexdigit:
	+sub '0'
	cmp #10
	bmi smove_h1
	+sub 'A'-'0'-10
smove_h1:
	rts

smove_subst:
	!byte 7,8,'C','D',27,12,'G','H','I','J','K'
	!byte 10,'M',NEW_LINE,'O','P',34,13,'S',9,'U',11
	!byte 'W','X','Y',0

+header ~pad, ~pad_n, smove_n, 3, "PAD"
	+code doconst
	+value _pad

+header ~unused, ~unused_n, pad_n, 6, "UNUSED"
	+forth
	+token lit
	+value MEMTOP
	+token here, sub, exit

+header ~fill, ~fill_n, unused_n, 4, "FILL"
	+forth
	+token swap, tor, swap
fill_1:
	+token rfrom, dup, qbranch
	+address fill_2
	+token oneminus, tor, twodup, cpoke, oneplus, branch
	+address fill_1
fill_2:
	+token twodrop, drop, exit

+header ~erase, ~erase_n, fill_n, 5, "ERASE"
	+forth
	+token zero, fill, exit

+header ~sstring, ~sstring_n, erase_n, 7, "/STRING"
	+forth
	+token rot, over, add, rot, rot, sub, exit

+header ~blank, ~blank_n, sstring_n, 5, "BLANK"
	+forth
	+token bl, fill, exit

+header ~sliteral, ~sliteral_n, blank_n, 8 + IMM_FLAG, "SLITERAL"
	+forth
	+token state, peek, qbranch
	+address sliteral_1
	+token compile, branch, fmark
	+token rot, rot
	+token dup, tor, here, dup, tor
	+token swap, dup, allot, cmove, fresolve
	+token compile, lit, rfrom
	+token comma, compile, lit, rfrom
	+token comma
sliteral_1:
	+token exit

+header ~qmark, ~qmark_n, sliteral_n, 1, "?"
	+forth
	+token peek, dot, exit

+header ~dots, ~dots_n, qmark_n, 2, ".S"
	+forth
	+token depth
dots_1:
	+token dup, qbranch
	+address dots_2
	+token dup, pick, dot, oneminus, branch
	+address dots_1
dots_2:
	+token drop, exit

+header ~ahead, ~ahead_n, dots_n, 5, "AHEAD"
	+forth
	+token fmark, exit

; The next two are non-standard but proposed for inclusion
+header ~place, ~place_n, ahead_n, 5 + NST_FLAG, "PLACE"
	+forth
	+token twodup, twotor, charplus, swap
	+token chars, move, tworfrom, cpoke, exit

+header ~plusplace, ~plusplace_n, place_n, 6 + NST_FLAG, "+PLACE"
	+forth
	+token dup, count, add, tor, twodup, cpeek
	+token add, swap, cpoke, rfrom, swap, move, exit

; ==============================================================================
; More words from the optional Double-Number word set

;
; : d= rot = >r = r> and ;
;

+header ~dequal, ~dequal_n, plusplace_n, 2, "D="
	+forth
	+token rot, equal, tor, equal, rfrom, and_op, exit

;
; : dmax 2over 2over d< if 2swap then 2drop ;
; : dmin 2over 2over d< invert if 2swap then 2drop ;
;

+header ~dmax, ~dmax_n, dequal_n, 4, "DMAX"
	+forth
	+token twoover, twoover, dless, qbranch
	+address dmax_1
	+token twoswap
dmax_1:
	+token twodrop, exit

+header ~dmin, ~dmin_n, dmax_n, 4, "DMIN"
	+forth
	+token twoover, twoover, dless, invert, qbranch
	+address dmin_1
	+token twoswap
dmin_1:
	+token twodrop, exit

;
; : d- dnegate d+ ;
; code d+
;

+header ~dsub, ~dsub_n, dmin_n, 2, "D-"
	+forth
	+token dnegate, dadd, exit

wlow = _scratch
whigh = _rscratch

+header ~dadd, ~dadd_n, dsub_n, 2, "D+"
	+code
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

+header ~dtwodiv, ~dtwodiv_n, dadd_n, 3, "D2/"
	+forth
	+token dup, one, and_op, lit
	+value 15
	+token lshift, swap, twodiv, swap
	+token rot, twodiv, or, swap, exit

;
; : d2* 2dup d+ ;
;

+header ~dtwomul, ~dtwomul_n, dtwodiv_n, 3, "D2*"
	+forth
	+token twodup, dadd, exit

+header ~duless, ~duless_n, dtwomul_n, 3, "DU<"
	+forth
	+token rot, twodup, equal, qbranch
	+address duless_1
	+token twodrop, uless, exit
duless_1:
	+token ugreater, qbranch
	+address duless_2
	+token twodrop, true, exit
duless_2:
	+token twodrop, false, exit

;
; : d0= or 0= ;
; : d0< nip 0< ;
; : d< rot > if 2drop true else < then ;
;

+header ~dzeroeq, ~dzeroeq_n, duless_n, 3, "D0="
	+forth
	+token or, zeroeq, exit

+header ~dzeroless, ~dzeroless_n, dzeroeq_n, 3, "D0<"
	+forth
	+token nip, zerolt, exit

+header ~dless, ~dless_n, dzeroless_n, 2, "D<"
	+forth
	+token rot, twodup, equal, qbranch
	+address dless_1
	+token twodrop, uless, exit
dless_1:
	+token greater, qbranch
	+address dless_2
	+token twodrop, true, exit
dless_2:
	+token twodrop, false, exit

;
; : d>s drop ;
;

+header ~dtos, ~dtos_n, dless_n, 3, "D>S"
	+forth
	+token drop, exit

;
; : 2constant create , , does> 2@ ;
;

+header ~dconstant, ~dconstant_n, dtos_n, 9, "2CONSTANT"
	+forth
	+token create, comma, comma, xcode
	!byte JSR_INSTR
	+token does, twopeek, exit

;
; : 2lit r@ 2@ r> 2+ 2+ >r ; nonstandard
; : 2literal ?comp state @ if compile 2lit , , then ; immediate
;

+header ~dlit, ~dlit_n, dconstant_n, 4 + NST_FLAG, "2LIT"
	+forth
	+token rat, twopeek, rfrom, twoplus, twoplus, tor, exit

+header ~dliteral, ~dliteral_n, dlit_n, 8 + IMM_FLAG, "2LITERAL"
	+forth
	+token qcomp, state, peek, qbranch
	+address dliteral_1
	+token compile, dlit
	+token comma, comma
dliteral_1:
	+token exit

;
; : 2rot 5 roll 5 roll ;
;

+header ~drot, ~drot_n, dliteral_n, 4, "2ROT"
	+forth
	+token lit
	+value 5
	+token roll, lit
	+value 5
	+token roll, exit

+header ~dvalue, ~dvalue_n, drot_n, 6, "2VALUE"
	+forth
	+token create, lit
	+value dovalue
	+token here, twominus, poke, lit
	+value dvalue_sem
	+token comma, comma, comma, exit
dvalue_sem:
	+token twopeek, twopoke, compdpoke

+header_internal ~compdpoke
	+forth
	+token compile, lit, comma, compile, twopoke, exit


;
; M*/ is an unusual word that uses three-cell numbers. It is possible to build it from the existing words
; To make it more clear, using some internal helpers:
; : t* ( ud,u -- ut) 2>r r@ m* 0 2r> m* d+ ;
; : t/ ( ut,u -- ud) dup >r um/mod r> swap >r um/mod nip r> ;
; : normsign ( d,n -- ud,u,n ) 2dup xor >r abs rot rot dabs rot r> ;
;
tmult:
	+forth
	+token twotor, rat, ummult, zero, tworfrom, ummult, dadd, exit
tdiv:
	+forth
	+token dup, tor, ummod, rfrom, swap
	+token tor, ummod, nip, rfrom, exit
normsign:
	+forth
	+token twodup, xor, tor, abs
	+token rot, rot, dabs, rot, rfrom, exit

;
; : m*/ >r normsign r> swap >r >r t* r> t/ r> 0< if dnegate then ;
;

+header ~mmuldiv, ~mmuldiv_n, dvalue_n, 3, "M*/"
	+forth
	+token tor, normsign, rfrom, swap, tor, tor
	+token tmult, rfrom, tdiv, rfrom, zerolt, qbranch
	+address mmuldiv_1
	+token dnegate
mmuldiv_1:
	+token exit

; ==============================================================================

+header ~env_counted, ~env_counted_n, mmuldiv_n, 15 + NST_FLAG, "/COUNTED-STRING"
	+code doconst
	+value 255
				
+header ~env_hold, ~env_hold_n, env_counted_n, 5 + NST_FLAG, "/HOLD"
	+code doconst
	+value 98
				
+header ~env_pad, ~env_pad_n, env_hold_n, 4 + NST_FLAG, "/PAD"
	+code doconst
	+value 100
				
+header ~env_bits, ~env_bits_n, env_pad_n, 17 + NST_FLAG, "ADDRESS-UNIT-BITS"
	+code doconst
	+value 16
				
+header ~env_floored, ~env_floored_n, env_bits_n, 7 + NST_FLAG, "FLOORED"
	+code doconst
	+value VAL_TRUE
				
+header ~env_maxchar, ~env_maxchar_n, env_floored_n, 8 + NST_FLAG, "MAX-CHAR"
	+code doconst
	+value 255
				
+header ~env_maxd, ~env_maxd_n, env_maxchar_n, 5 + NST_FLAG, "MAX-D"
	+forth
	+token dlit
	+value $FFFF
	+value $7FFF
	+token exit
				
+header ~env_maxn, ~env_maxn_n, env_maxd_n, 5 + NST_FLAG, "MAX-N"
	+code doconst
	+value $7FFF
				
+header ~env_maxu, ~env_maxu_n, env_maxn_n, 5 + NST_FLAG, "MAX-U"
	+code doconst
	+value $FFFF
				
+header ~env_maxud, ~env_maxud_n, env_maxu_n, 6 + NST_FLAG, "MAX-UD"
	+forth
	+token dlit
	+value $FFFF
	+value $FFFF
	+token exit
				
+header ~env_rstack, ~env_rstack_n, env_maxud_n, 18 + NST_FLAG, "RETURN-STACK-CELLS"
	+code doconst
	+value RSIZE / 2
				
+header ~env_stack, ~env_stack_n, env_rstack_n, 11 + NST_FLAG, "STACK-CELLS"
	+code doconst
	+value (DSIZE - 4*SSAFE) / 2 + 1	; Note the extra cell for _dtop
				
+header ~environmentq, ~environmentq_n, env_stack_n, 12, "ENVIRONMENT?"
	+forth
	+token xfind, dup, qbranch
	+address environmentq_1
	+token count, lit
	+value NAMEMASK
	+token and_op, add, twoplus, execute, true
environmentq_1:
	+token exit

; ==============================================================================
; Optional File-Access word set

; Forth standard makes assumptions about I/O capabilities that are simply not true
; for most 8-bit systems. Implementing as much as possible to get the system going

_devnum = _rscratch
_secondary = _wscratch

; Full equivalent to C64 OPEN, not exposed to dictionary yet
; Note that it requires 5 stack parameters as the string is enchoded as (c_addr,u)
; Top of data stack will have filenum or 0 on error
+header_internal ~c64open:
	+code
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
+header_internal ~c64close:
	+code
	+dpop
	jsr CLOSE
	jmp next


+header ~bin, ~bin_n, environmentq_n, 3, "BIN"
	+forth
	+token exit		; taking the recommendation and handling all files as binary

+header ~ro, ~ro_n, bin_n, 3, "R/O"
	+code doconst
	+value ro_v
ro_v:
	+string 4, ",S,R"

+header ~wo, ~wo_n, ro_n, 3, "W/O"
	+code doconst
	+value wo_v
wo_v:
	+string 4, ",S,W"

; This may not be supported on C64, making it identical to W/O
+header ~rw, ~rw_n, wo_n, 3, "R/W"
	+code doconst
	+value wo_v

; For C64 OPEN-FILE and CREATE-FILE are identical
+header ~createfile, ~createfile_n, rw_n, 11, "CREATE-FILE"
	+forth
	+token openfile, exit

+header ~openfile, ~openfile_n, createfile_n, 9, "OPEN-FILE"
	+forth
	+token tor, lit
	+value of_1
	+token count, lit
	+value _fnamebuf
	+token place, lit
	+value _fnamebuf
	+token plusplace
	+token rfrom, count, lit
	+value _fnamebuf
	+token plusplace, lit
	+value _openfiles
	+token peek, freebit
	+token dup, zerone, qbranch
	+address of_2
	+token lit
	+value 8
	+token over, lit
	+value _fnamebuf
	+token count, c64open, dup, lit
	+value _openfiles
	+token setbit, dup, zeroeq
of_2:
	+token exit
of_1:
	+string 3, "O0:"

+header ~closefile, ~closefile_n, openfile_n, 10, "CLOSE-FILE"
	+forth
	+token dup, lit
	+value _openfiles
	+token clearbit, c64close, zero, exit

; C64 equivalent: OPEN 1,8,15,"S0:Name":CLOSE 1
+header ~deletefile, ~deletefile_n, closefile_n, 11, "DELETE-FILE"
	+forth
	+token lit
	+value df_1
	+token count, lit
	+value _fnamebuf
	+token place, lit
	+value _fnamebuf
	+token plusplace
	+token one, lit
	+value 8
	+token lit
	+value 15
	+token lit
	+value _fnamebuf
	+token count, c64open
	+token one, notequal, one, c64close, exit
df_1:
	+string 3, "S0:"

; C64 equivalent: OPEN 1,8,15,"R0:NewName=OldName":CLOSE 1
+header ~renamefile, ~renamefile_n, deletefile_n, 11, "RENAME-FILE"	; Note that this is the only word that uses PAD
	+forth
	+token lit
	+value rf_1
	+token count, lit
	+value _fnamebuf
	+token place
	+token lit
	+value _fnamebuf
	+token plusplace
	+token lit
	+value rf_2
	+token count, lit
	+value _fnamebuf
	+token plusplace
	+token lit
	+value _fnamebuf
	+token plusplace
	+token one, lit
	+value 8
	+token lit
	+value 15
	+token lit
	+value _fnamebuf
	+token count, c64open
	+token one, notequal, one, c64close, exit
rf_1:
	+string 3, "R0:"
rf_2:
	+string 1, "="

; Cannot be implemented on C64
+header ~resizefile, ~resizefile_n, renamefile_n, 11, "RESIZE-FILE"
	+forth
	+token drop, twodrop, minusone, exit

; Cannot be implemented on C64
+header ~repositionfile, ~repositionfile_n, resizefile_n, 15, "REPOSITION-FILE"
	+forth
	+token drop, twodrop, minusone, exit

; Cannot be implemented on C64
+header ~fileposition, ~fileposition_n, repositionfile_n, 13, "FILE-POSITION"
	+forth
	+token drop, zero, zero, minusone, exit

; Cannot be implemented on C64
+header ~filesize, ~filesize_n, fileposition_n, 9, "FILE-SIZE"
	+forth
	+token drop, zero, zero, minusone, exit

; A simplistic implementation to test for existence
+header ~filestatus, ~filestatus_n, filesize_n, 11, "FILE-STATUS"
	+forth
	+token ro, openfile, qbranch
	+address filestatus_1
	+token true, exit
filestatus_1:
	+token closefile, zero, exit

+header_internal ~setread
	+code
	+dpop
	tax
	jsr CHKIN
	jmp next

+header_internal ~xreadchar
	+code
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

+header_internal ~xreadbyte
	+code
	jsr CHRIN
	ldx #0
	+dpush
	jmp next

+header_internal ~iseof
	+code
	jsr READST
	and #64
	tax
	+dpush
	jmp next

; There is something odd in EOF logic on X16 (maybe on C64 as well). The EOF state is not kept with
; the handle properly, so it will be false after CHRIN triggering false reads on the line after
; the EOF (the line with EOF cannot really return that state per Forth standard).
; As a workaround, skipping zero chars in READ-LINE, these should not occur in text files anyway.

+header ~readline, ~readline_n, filestatus_n, 9, "READ-LINE"
	+forth
	+token setread, swap, dup, rot, add, over					; c-addr, c-addr-limit, current
readline_1:
	+token twodup, swap, uless, qbranch
	+address readline_4				; buffer full?
readline_7:
	+token iseof, zeroeq, qbranch
	+address readline_2
	+token xreadchar, qdup, qbranch
	+address readline_7 ; EOF workaround
	+token dup, lit
	+value NEW_LINE
	+token notequal, qbranch
	+address readline_3		; end of line
	+token over, cpoke, oneplus, branch
	+address readline_1 
readline_2:
	+token swap, drop, swap, sub, dup, zeroeq, qbranch
	+address readline_5
	+token false, branch
	+address readline_6
readline_3:
	+token drop
readline_4:
	+token swap, drop, swap, sub
readline_5:
	+token true
readline_6:
	+token zero, setread, zero, exit

+header ~readfile, ~readfile_n, readline_n, 9, "READ-FILE"
	+forth
	+token setread, swap, dup, rot, add, over					; c-addr, c-addr-limit, current
readfile_1:
	+token twodup, swap, uless, qbranch
	+address readfile_3				; buffer full?
	+token iseof, zeroeq, qbranch
	+address readfile_2
	+token xreadbyte		; end of file?
	+token over, cpoke, oneplus, branch
	+address readfile_1 
readfile_2:
readfile_3:
	+token swap, drop, swap, sub, zero, zero, setread, exit

+header_internal ~setwrite
	+code
	+dpop
	tax
	jsr CHKOUT
	jmp next

xputchar = emit

+header ~writeline, ~writeline_n, readfile_n, 10, "WRITE-LINE"
	+forth
	+token dup, tor, writefile, rfrom, setwrite, lit
	+value NEW_LINE
	+token xputchar, zero, setwrite, exit

; This can be implemented using KERNAL SAVE, but the corresponding READ-LINE cannot be implemented
; with KERNAL LOAD. Leaving it as is for now.
+header ~writefile, ~writefile_n, writeline_n, 10, "WRITE-FILE"
	+forth
	+token setwrite
writefile_1:
	+token dup, zerone, qbranch
	+address writefile_2
	+token swap, dup, cpeek, xputchar, oneplus, swap, oneminus, branch
	+address writefile_1
writefile_2:
	+token twodrop, zero, setwrite, zero, exit

; Not needed on C64
+header ~flushfile, ~flushfile_n, writefile_n, 10, "FLUSH-FILE"
	+forth
	+token zero, exit

+header ~includefile, ~includefile_n, flushfile_n, 12, "INCLUDE-FILE"
	+forth
	+token lit
	+value _ibufcount
	+token peek, lit
	+value 7
	+token greater, qbranch
	+address includefile_1
	+token xabortq
	+string 17, "TOO MANY INCLUDES"
includefile_1:
	+token lit
	+value _source
	+token peek
	+token twominus, zero, over, poke	; two more entries to keep fileposition before the last refill
	+token twominus, zero, over, poke
	+token twominus, zero, over, poke
	+token twominus, lit
	+value _ibuf
	+token lit
	+value _ibufcount
	+token peek, lit
	+value 100
	+token mult, add, over, poke
	+token twominus, zero, over, poke
	+token twominus, swap, over, poke
	+token twominus, lit
	+value 6
	+token over, poke
	+token lit
	+value _source
	+token poke
	+token lit
	+value _ibufcount
	+token dup, peek, oneplus, swap, poke
	+token state, qbranch
	+address includefile_2
	+token interpret
includefile_2:
	+token exit

+header ~include, ~include_n, includefile_n, 7, "INCLUDE"
	+forth
	+token parsename, included, exit

; TODO - should produce an error on non-existing file. Currently OPEN-FILE does not report that properly
+header ~included, ~included_n, include_n, 8, "INCLUDED"
	+forth
	+token twodup, filestatus, nip, qbranch
	+address included_1
	+token twodrop, exit
included_1:
	+token twodup, xcreate, ro, openfile, qbranch
	+address included_2
	+token drop, exit
included_2:
	+token includefile, exit

+header ~require, ~require_n, included_n, 7, "REQUIRE"
	+forth
	+token parsename, required, exit

+header ~required, ~required_n, require_n, 8, "REQUIRED"
	+forth
	+token twodup, xfind, zeroeq, qbranch
	+address required_1
	+token included, exit
required_1:
	+token twodrop, exit

; ==============================================================================
; Small subset from the optional Facility word set

+header ~beginstructure, ~beginstructure_n, required_n, 15, "BEGIN-STRUCTURE"
	+forth
	+token create, here, zero, zero, comma, xcode
	!byte JSR_INSTR
	+token does, peek, exit

+header ~endstructure, ~endstructure_n, beginstructure_n, 13, "END-STRUCTURE"
	+forth
	+token swap, poke, exit
				
+header ~addfield, ~addfield_n, endstructure_n, 6, "+FIELD"
	+forth
	+token create, over, comma, add, xcode
	!byte JSR_INSTR
	+token does, peek, add, exit
			
+header ~field, ~field_n, addfield_n, 6, "FIELD:"
	+forth
	+token two, addfield, exit

+header ~cfield, ~cfield_n, field_n, 7, "CFIELD:"
	+forth
	+token one, addfield, exit

; ==============================================================================
; The main system loop. Intentionally placing it last so the listing of words will start with it

+header ~forth_system, ~forth_system_n, cfield_n, 12 + NST_FLAG, "FORTH-SYSTEM"
	+forth
forth_system_c:
	+token lit
	+value banner_text
	+token count, type, cr
	+token decimal, false, state, poke, xsst
	+token lit
	+value autorun
	+token count, included, branch
	+address forth_system_1
forth_system_r:
	+token decimal, false, state, poke, xsst
forth_system_1:
	+token interpret, branch
	+address forth_system_1
banner_text:
	+string 14, "FORTH TX16 1.0"
autorun:
	+string 11, "AUTORUN.FTH"

; ==============================================================================

end_of_image:

