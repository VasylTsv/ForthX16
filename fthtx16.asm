; Forth system for Commander X16 - port of Forth Model T
; Forth system for Commander X16 - port of Forth Model T
; by Vasyl Tsvirkunov
; At this point the compliance status is:
; Forth-2012 System
; Providing the Core Extensions word set
; - Providing names from the Programming-Tools word set
; - Providing names from the String word set (need -TRAILING and SEARCH for compliance, no Extensions)
; Providing the Double-Number word set
; Providing the Double-Number Extensions word set
; Providing the File Access word set (limited functionality)
; Providing the File Access Extensions word set
; - Providing names from the Facility word set
; Providing the Search-Order word set (pending)
; Providing the Search-Order Extensions word set (pending)
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
; Removed SAVE-SYSTEM - may come back in a different variation
; Removed non-standard flag support, did not serve much purpose, and caused implementation issues
; Removed ENVIRONMENT? queries, not required by standard, useless
; Changed some non-standard internal word semantics for better code reuse ((FIND), (CREATE))
; Made a number of non-standard words internal

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
TOKEN_COUNT = $0600	; Maximal number of available tokens in token threaded model

; In case if these get moved - important requirement, stacks must be aligned on 16-bit boundary;
; the token table must be aligned on page boundary for performance reasons
TOKENS = $9f00 - 2*TOKEN_COUNT
RSTACK = TOKENS - RSIZE
RSTACK_INIT = RSTACK + RSIZE - 2
DSTACK = $0400
DSTACK_INIT = DSTACK + DSIZE - 2*SSAFE - 2

STACKLIMIT = DSIZE - 4*SSAFE

NAMEMASK = 31 ; vocabulary entries can have up to 32 characters
IMM_FLAG = 128 ; flag for immediate words
VAL_TRUE = -1 ; required by current standard
VAL_FALSE = 0
JSR_INSTR = $20 ; DOES> needs to emit JSR opcode
JMP_INSTR = $4C ; in direct threading each Forth word stars with JMP CALL
RTS_INSTR = $60	; in size-optimized direct threading this is the entire prolog of Forth calls

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
_latest		= _base+2
_current	= _latest+2
_state		= _current+2
_sflip		= _state+2		; flip-flop for S" buffer selection
_ibufcount	= _sflip+2		;number of used buffers
_source 	= _ibufcount+2 ; pointer to the current source
_openfiles	= _source+2 	; bitfield for files currently open to translate from C64 to Forth opening semantics

_stopcheck	= _openfiles+2

_hightoken	= _stopcheck+2

; Search-Order support - 16 values (XTs) and number of items in the list
_order = _hightoken+2		; this may be moved to upper memory
_orderitems = _order+32

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
!macro header ~.token, ~.label_n, .prev_token, .prev_n, .s, .name {
.label_n:
	!byte .s
	!text .name
	!if .label_n-.prev_n < 128 {
		!byte .label_n-.prev_n
	} else {
		!if .prev_n = 0 {
			!byte 0
		} else {
			!byte ((.label_n-.prev_n) >> 8) | $80, (.label_n-.prev_n) & $FF
		}
	}
.token = .prev_token + 1
}

!macro header ~.token, ~.label_n {
.label_n:
	!byte 0
	!byte 0
.token = 15	; tokens 0-15 have special meaning extending tokens past 8 bit
}


; Internal words (native code inserts) that should not show in the vocabulary.
; Anything that starts with +code or +forth should have one of the header
; varieties.

!macro header ~.token, ~.label_n, .prev_token, .prev_n {
.label_n:
	!byte 0
	!if .label_n-.prev_n < 128 {
		!byte .label_n-.prev_n
	} else {
		!if .prev_n = 0 {
			!byte 0
		} else {
			!byte ((.label_n-.prev_n) >> 8) | $80, (.label_n-.prev_n) & $FF
		}
	}
.token = .prev_token + 1
}

; Beginning of a compiled Forth word. Tokens follow
!macro forth {
	rts
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

; Similarly, 8-bite value for BLIT
!macro bvalue .v {
	!byte .v
}

; Address (typically for preceeding BRANCH or ?BRANCH tokens)
!macro address .a {
	!word .a
}

; Short relative address for BBRANCH and ?BBRANCH
!macro baddress .a {
	!byte .a-*
}

; Pascal-style string
!macro string .l, .s {
	!byte .l
	!text .s
}

; One to eight tokens per line
!macro token .t1 {
	!byte .t1
}

!macro token .t1, .t2 {
	!byte .t1, .t2
}

!macro token .t1, .t2, .t3 {
	!byte .t1, .t2, .t3
}

!macro token .t1, .t2, .t3, .t4 {
	!byte .t1, .t2, .t3, .t4
}

!macro token .t1, .t2, .t3, .t4, .t5 {
	!byte .t1, .t2, .t3, .t4, .t5
}

!macro token .t1, .t2, .t3, .t4, .t5, .t6 {
	!byte .t1, .t2, .t3, .t4, .t5, .t6
}

!macro token .t1, .t2, .t3, .t4, .t5, .t6, .t7 {
	!byte .t1, .t2, .t3, .t4, .t5, .t6, .t7
}

!macro token .t1, .t2, .t3, .t4, .t5, .t6, .t7, .t8 {
	!byte .t1, .t2, .t3, .t4, .t5, .t6, .t7, .t8
}


; Miscellaneous buffers allocated below the top of the accessible memory
_ibuf = RSTACK - 700 		; seven 100-char buffers for INCLUDE-FILE
_sbuf = _ibuf - 200 		; two 100-char buffers for S" / S\"
_fnamebuf = _sbuf - 100 	; buffer for filename storage only

_tib = _fnamebuf - 100		; input buffer (reserving 100 bytes although only 81 are really needed)
_wordbuf = _tib - 100 		; buffer to hold result of WORD (reserving 100 bytes)
_pad = _wordbuf - 100		; PAD buffer
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

	lda #<forth_system_c
	ldx #>forth_system_c
	+stax _ri				; _w does not need to be initialized

	+init_rstack
	+init_dstack

	ldx #0
	txa
	+stax _state
	+stax _sflip
	+stax _ibufcount

	sta _tib
	sta _wordbuf

	lda #10
	+stax _base

	lda #7			; do not try to open 0-2
	+stax _openfiles

	lda #<forth_system_n
	ldx #>forth_system_n
	+stax _latest
	lda #<end_of_image
	ldx #>end_of_image
	+stax _here

; In token threaded code we need to generate the mapping of tokens to addresses
	jsr generate_token_table

	lda #<forth_system
	ldx #>forth_system
	+stax _hightoken

; Prep for the Forth words, so RTS will jump to CALL. As call starts with the same
; sequence, it will be consistently repeatable while saving 2 bytes per Forth word
	lda #>call-1
	pha
	lda #<call-1
	pha

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
; +header ~example, ~example_n, link_last, link_last_n, 6, "Example"
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
;	RI += 1		; token size(!)
;	goto mem(W)	; ->token

next:
	ldx #>TOKENS		; note that LSB is assumed to be 0
next_ext:
	ldy #0
	lda (_ri),y
	asl
	bcc +
	inx
+:
	inc _ri
	bne +
	inc _ri+1
+:
	+stax _scratch
	lda (_scratch),y
	sta _w
	iny
	lda (_scratch),y
	sta _w+1
	jmp (_w)

; Special entry for tokens 0-16 - the jump from next will lead directly here per the token table.
; The assumption is that _scratch has 2x the token id, so the exact page offset required. Token
; 0 is not useful in this context, but it is harmless
prefix_token:
	clc
	lda #>TOKENS
	adc _scratch
	tax
	bne next_ext

; CALL - this will execute the parameters at W
; 	rpush(RI)
;	RI = W+1	; offset for RTS
;	goto NEXT

call:
; prep for the subsequent call
	lda #>call-1
	pha
	lda #<call-1
	pha

; STOP key handler will trigger on every 256th execution of call
	inc _stopcheck
	bne +
	jsr STOP
	bne +
	jmp abort_c
+:

	+ldax _ri
	+rpush
	+ldax _w
	+incax 1
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
invokeax:
	asl
	sta _scratch
	txa
	rol
	adc #>TOKENS
	sta _scratch+1
	ldy #0
	lda (_scratch),y
	sta _w
	iny
	lda (_scratch),y
	sta _w+1
	jmp (_w)

; CREATED - push the PFA on the stack (default semantics of a word after CREATE)
;	push(W+3)	; Note the offset 3 here. "Created" words have JMP CREATED prolog, not RTS!
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
;	push(W+3)	; offset for jmp call - see the above note
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
;	W = mem(W+3)	; offset for jmp dodefer
;	goto mem(W)

dodefer:
	ldy #4
	lda (_w),y
	tax
	dey
	lda (_w),y
	jmp invokeax

; DOVALUE - semantics of a VALUE
;	Assumes a very particular structure: pointer to semantics block followed by value bytes. Semantics block contains
;	three addresses: read semantics, write semantics, compilation semantics
;	push(W+5)			; offset for jmp dovalue and one pointer to semantics block
;	W = mem(mem(W+3))	; offset for jmp dovalue
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
	sta _rscratch	; rscratch -> VALUE word semantics block

	ldy #1
	lda (_rscratch),y
	tax
	dey
	lda (_rscratch),y
	
	jmp invokeax

; A number of Forth words have constant semantics. Typical systems define CONSTANT using DOES> but that wastes a few
; bytes for the call. Using a separate semantic word instead.
;	push mem(W+3)	; offset for jmp doconst
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
	bne +
	dec _rstack+1
+:
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
	bne +
	inc _rstack+1
+:
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
	bne +
	dec _dstack+1
+:
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
	bne +
	inc _dstack+1
+:
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

generate_token_table:
	; start scanning from the last word NFA
	lda #<forth_system_n
	ldx #>forth_system_n
	+stax _rscratch
	
	; set current token to be the last
	clc
	lda #<TOKENS
	adc #(forth_system<<1)&$ff		; last token id
	sta _scratch
	lda #>TOKENS
	adc #(forth_system<<1)>>8
	sta _scratch+1

gtt_next:
	; get the link
	lda #0
	sta _wscratch+1
	tay
	lda (_rscratch),y
	and #NAMEMASK
	tay
	iny					; name + name byte, now pointing at the LFA
	lda (_rscratch),y
	beq gtt_done
	bpl gtt_offsetok
	and #$7f
	sta _wscratch+1
	iny
	lda (_rscratch),y
gtt_offsetok:
	iny
	sta _wscratch

	; get the actual address
	tya
	clc
	adc _rscratch
	tax
	lda #0
	adc _rscratch+1
	
	; set the current token to that address
	ldy #1
	sta (_scratch),y
	dey
	txa
	sta (_scratch),y

	; step to previous token
	lda _scratch
	bne +
	dec _scratch+1
+:
	dec _scratch
	dec _scratch

	; step to the previous NFA
	sec
	lda _rscratch
	sbc _wscratch
	sta _rscratch
	lda _rscratch+1
	sbc _wscratch+1
	sta _rscratch+1
	jmp gtt_next

gtt_done:

	; fill in the bottom 16 entries with references to prefix_token
	lda #<TOKENS
	ldx #>TOKENS
	+stax _wscratch
	ldy #31
-:
	lda #>prefix_token
	sta (_wscratch),y
	dey
	lda #<prefix_token
	sta (_wscratch),y
	dey
	bpl -

	rts

; ==============================================================================
; Forth vocabulary starts here
+header ~startup, ~startup_n

; ==============================================================================
; code exit
; code execute
; : quit (sst) ;code
; : abort begin depth 0> while drop again begin depth <0 while 0 again quit ;
;

; EXIT is used to return from any word.
+header ~exit, ~exit_n, startup, startup_n, 4, "EXIT"
	+code return

; Execute the word by address on the stack
+header ~execute, ~execute_n, exit, exit_n, 7, "EXECUTE"
	+code invoke

; TODO: should also call (SST) in QUIT
; Reset data stack and perform QUIT.
+header ~abort, ~abort_n, execute, execute_n, 5, "ABORT"
	+code
abort_c:
	+init_dstack
	jmp quit_c

;
; : (sst) _sourcestack
;         2- 0 over ! 2- _tib over ! 2- 0 over ! 2- 0 over ! 2- 4 over !
;         _source ! 0 dup _sflip ! _ibufcount ! ; nonstandard
;

+header ~xsst, ~xsst_n, abort, abort_n			; Reset source stack
	+forth
	+token lit
	+value _sourcestack
	+token twominus, zero, over, poke			; #TIB
	+token twominus, lit
	+value _tib
	+token over, poke							; TIB
	+token twominus, zero, over, poke			; >IN
	+token twominus, zero, over, poke			; SOURCE-ID
	+token twominus, blit
	+bvalue $04
	+token over, poke		; standard input has 4 parameters: 0, >IN, TIB, #TIB
	+token lit
	+value _source
	+token poke
	+token zero, dup, lit
	+value _sflip
	+token poke, lit
	+value _ibufcount
	+token poke, exit

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

+header ~zero, ~zero_n, xsst, xsst_n, 1, "0"
	+code doconst
	+value $0000

+header ~one, ~one_n, zero, zero_n, 1, "1"
	+code doconst
	+value $0001

+header ~two, ~two_n, one, one_n, 1, "2"
	+code doconst
	+value $0002

+header ~minusone, ~minusone_n, two, two_n, 2, "-1"
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

+header ~add, ~add_n, minusone, minusone_n, 1, "+"
	+code
	+dpop
	clc
	adc _dtop
	sta _dtop
	txa
	adc _dtop+1
	sta _dtop+1
	jmp next

+header ~sub, ~sub_n, add, add_n, 1, "-"
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

+header ~mult, ~mult_n, sub, sub_n, 1, "*"
	+forth
	+token mmult, drop, exit

+header ~divmod, ~divmod_n, mult, mult_n, 4, "/MOD"
	+forth
	+token tor, stod, rfrom, smrem, exit

+header ~multdivmod, ~multdivmod_n, divmod, divmod_n, 5, "*/MOD"
	+forth
	+token tor, mmult, rfrom, smrem, exit

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

+header ~abs, ~abs_n, multdivmod, multdivmod_n, 3, "ABS"
	+code
	lda _dtop+1
	bmi negate_c
	jmp next

+header ~negate, ~negate_n, abs, abs_n, 6, "NEGATE"
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

+header ~oneplus, ~oneplus_n, negate, negate_n, 2, "1+"
	+code
	inc _dtop
	bne +
	inc _dtop+1
+:
	jmp next

+header ~oneminus, ~oneminus_n, oneplus, oneplus_n, 2, "1-"
	+code
	lda _dtop
	bne +
	dec _dtop+1
+:
	dec _dtop
	jmp next

+header ~twoplus, ~twoplus_n, oneminus, oneminus_n, 2, "2+"
	+code
	+ldax _dtop
	+incax 2
	+stax _dtop
	jmp next

+header ~twominus, ~twominus_n, twoplus, twoplus_n, 2, "2-"
	+code
	+ldax _dtop
	+decax 2
	+stax _dtop
	jmp next

+header ~twodiv, ~twodiv_n, twominus, twominus_n, 2, "2/"
	+code
	lda _dtop+1
	cmp #$80		; 6502 does not have native arithmetic shift right 
	ror _dtop+1
	ror _dtop
	jmp next

+header ~twomult, ~twomult_n, twodiv, twodiv_n, 2, "2*"
	+code
	asl _dtop
	rol _dtop+1
	jmp next

;
; code lshift
; code rshift
;

+header ~lshift, ~lshift_n, twomult, twomult_n, 6, "LSHIFT"
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
			
+header ~rshift, ~rshift_n, lshift, lshift_n, 6, "RSHIFT"
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

+header ~stod, ~stod_n, rshift, rshift_n, 3, "S>D"
	+forth
	+token dup, zerolt, qbbranch
	+baddress stod_1
	+token minusone, exit
stod_1:
	+token zero, exit

; Optional Double=number word set
+header ~dnegate, ~dnegate_n, stod, stod_n, 7, "DNEGATE"
	+forth
	+token invert, swap, invert, swap, one, mplus, exit 

; Optional Double-numbler word set
+header ~dabs, ~dabs_n, dnegate, dnegate_n, 4, "DABS"
	+forth
	+token dup, zerolt, qbbranch
	+baddress dabs_1
	+token dnegate
dabs_1:
	+token exit

;
; : sm/rem 2dup xor >r ( Sign of the quotient) over >r ( Sign of the remainder)
;          abs >r dabs r> um/mod
;          swap r> 0< if negate then
;          swap r> 0< if negate then ;
;

+header ~smrem, ~smrem_n, dabs, dabs_n, 6, "SM/REM"
	+forth
	+token twodup, xor, tor, over, tor
	+token abs, tor, dabs, rfrom, ummod
	+token swap, rfrom, zerolt, qbbranch
	+baddress smrem_1
	+token negate
smrem_1:
	+token swap, rfrom, zerolt, qbbranch
	+baddress smrem_2
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

+header ~ummod, ~ummod_n, smrem, smrem_n, 6, "UM/MOD"
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

+header ~udmod, ~udmod_n, ummod, ummod_n, 6, "UD/MOD"
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

+header ~ummult, ~ummult_n, udmod, udmod_n, 3, "UM*"
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

+header ~mmult, ~mmult_n, ummult, ummult_n, 2, "M*"
	+forth
	+token twodup, xor, tor, abs, swap
	+token abs, ummult, rfrom, zerolt, qbbranch
	+baddress mmult_1
	+token dnegate
mmult_1:
	+token exit

+header ~udmult, ~udmult_n, mmult, mmult_n, 3, "UD*"
	+forth
	+token dup, tor, ummult, drop, swap
	+token rfrom, ummult, rot, add, exit

;
; : m+ s>d d+ ;
;

; From the optional Double-number word set
+header ~mplus, ~mplus_n, udmult, udmult_n, 2, "M+"
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

+header ~and_op, ~and_n, mplus, mplus_n, 3, "AND"
	+code
	+dpop
	and _dtop
	sta _dtop
	txa
	and _dtop+1
	sta _dtop+1
	jmp next

+header ~or, ~or_n, and_op, and_n, 2, "OR"
	+code
	+dpop
	ora _dtop
	sta _dtop
	txa
	ora _dtop+1
	sta _dtop+1
	jmp next

+header ~xor, ~xor_n, or, or_n, 3, "XOR"
	+code
	+dpop
	eor _dtop
	sta _dtop
	txa
	eor _dtop+1
	sta _dtop+1
	jmp next

; Note that NOT has been removed from the standard.
+header ~invert, ~invert_n, xor, xor_n, 6, "INVERT"
	+forth
	+token minusone, xor, exit

; Find lowest zero (free) bit index
+header ~freebit, ~freebit_n, invert, invert_n, 7, "FREEBIT"
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

+header ~setbit, ~setbit_n, freebit, freebit_n, 6, "SETBIT"
	+forth
	+token dup, peek, rot, one, swap
	+token lshift, or, swap, poke, exit

+header ~clearbit, ~clearbit_n, setbit, setbit_n, 8, "CLEARBIT"
	+forth
	+token dup, peek, rot, one, swap, lshift
	+token invert, and_op, swap, poke, exit

; ==============================================================================
; Comparisons

;
; code 0=
; code 0<
;

+header ~zeroeq, ~zeroeq_n, clearbit, clearbit_n, 2, "0="
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

+header ~zerolt, ~zerolt_n, zeroeq, zeroeq_n, 2, "0<"
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

+header ~zerogt, ~zerogt_n, zerolt, zerolt_n, 2, "0>"
	+forth
	+token zero, swap, less, exit
			
+header ~zerone, ~zerone_n, zerogt, zerogt_n, 3, "0<>"
	+forth
	+token zeroeq, zeroeq, exit

+header ~equal, ~equal_n, zerone, zerone_n, 1, "="
	+forth
	+token sub, zeroeq, exit

+header ~notequal, ~notequal_n, equal, equal_n, 2, "<>"
	+forth
	+token sub, zerone, exit


;
; code <
;
; Careful here. Some implementations have it as ": < - 0< ;" and it works... sometimes.
; Signed comparison on 6502 is surprisingly non-trivial. Refer to http://www.6502.org/tutorials/compare_beyond.html
; for details and tutorial
+header ~less, ~less_n, notequal, notequal_n, 1, "<"
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

+header ~greater, ~greater_n, less, less_n, 1, ">"
	+forth
	+token swap, less, exit


;
;	code u<
;
+header ~uless, ~uless_n, greater, greater_n, 2, "U<"
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

+header ~ugreater, ~ugreater_n, uless, uless_n, 2, "U>"
	+forth
	+token swap, uless, exit

;
;	-1 constant true
;	0 constant false
;

+header ~true, ~true_n, ugreater, ugreater_n, 4, "TRUE"
	+code doconst
	+value VAL_TRUE

+header ~false, ~false_n, true, true_n, 5, "FALSE"
	+code doconst
	+value VAL_FALSE


; ==============================================================================
; Base stack operations.

;
;	code dup
;	code drop
;	code over
;	code swap
;

+header ~dup, ~dup_n, false, false_n, 3, "DUP"
	+code
	+ldax _dtop
	+dpush
	jmp next

+header ~drop, ~drop_n, dup, dup_n, 4, "DROP"
	+code
	+dpop
	jmp next

+header ~over, ~over_n, drop, drop_n, 4, "OVER"
	+code
	ldy #3
	lda (_dstack),y
	tax
	dey
	lda (_dstack),y
	+dpush
	jmp next

+header ~swap, ~swap_n, over, over_n, 4, "SWAP"
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

+header ~nip, ~nip_n, swap, swap_n, 3, "NIP"
	+forth
	+token swap, drop, exit

;
; : rot >r swap r> swap ;
;

+header ~rot, ~rot_n, nip, nip_n, 3, "ROT"
	+forth
	+token tor, swap, rfrom, swap, exit

;
;	code pick
;	code roll ; using reference implementation from forth-standard.org instead
;
+header ~pick, ~pick_n, rot, rot_n, 4, "PICK"
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

+header ~roll, ~roll_n, pick, pick_n, 4, "ROLL"
	+forth
	+token qdup, qbbranch
	+baddress roll_1 ; Using reference implementation
	+token swap, tor, oneminus, roll, rfrom, swap
roll_1:
	+token exit

;
;	code depth
;

+header ~depth, ~depth_n, roll, roll_n, 5, "DEPTH"
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

+header ~twodrop, ~twodrop_n, depth, depth_n, 5, "2DROP"
	+forth
	+token drop, drop, exit

+header ~twodup, ~twodup_n, twodrop, twodrop_n, 4, "2DUP"
	+forth
	+token over, over, exit

+header ~twoswap, ~twoswap_n, twodup, twodup_n, 5, "2SWAP"
	+forth
	+token tor, rot, rot, rfrom, rot, rot, exit

+header ~twoover, ~twoover_n, twoswap, twoswap_n, 5, "2OVER"
	+forth
	+token tor, tor, twodup, rfrom, rfrom, twoswap, exit

;
;	: ?dup dup if dup then ;
;

+header ~qdup, ~qdup_n, twoover, twoover_n, 4, "?DUP"
	+forth
	+token dup, qbbranch
	+baddress qdup_1
	+token dup 
qdup_1:
	+token exit

; ==============================================================================
; DO/LOOP are considerably more complex so they are often implemented with helper words. In practical
; implementations these helpers are done in native code, so are I, J, and LEAVE.
; (DO) stores on the return stack: leaveaddr, limit, current, (ret) 
+header ~xdo, ~xdo_n, qdup, qdup_n, 4, "(DO)"
	+forth
	+token rfrom, dup, peek, tor	; forward ref for LEAVE
	+token rot, tor, swap, tor
	+token twoplus, tor				; step over the actual forward ref
	+token exit

+header ~xqdo, ~xqdo_n, xdo, xdo_n, 5, "(?DO)"
	+forth
	+token twodup, equal, qbbranch
	+baddress xqdo_1
	+token twodrop, rfrom, peek, tor, exit
xqdo_1:
	+token rfrom, dup, peek, tor	; forward ref for LEAVE
	+token rot, tor, swap, tor
	+token twoplus, tor				; step over the actual forward ref
	+token exit
			
; and (LOOP) adjusts the values on rstack or just drops the top three values from it to exit
+header ~xloop, ~xloop_n, xqdo, xqdo_n, 6, "(LOOP)"
	+forth
	+token rfrom				; return address is only needed to get the backref
	+token rfrom, oneplus			; new value of current
	+token rat, over, equal, qbbranch
	+baddress xloop_1
	+token drop, drop, rdrop, exit	; exit the loop (leaveaddr on the rstack)
xloop_1:
	+token tor, peek, tor, exit		; continue the loop

+header ~xploop, ~xploop_n, xloop, xloop_n, 7, "(+LOOP)"
	+forth
	+token rfrom, swap		; return address is only needed to get the backref / addr, step
	+token dup, rat, add			; preserve step value and get new value of current / addr, step, newcur
	+token rfrom, rat, sub			; diff limit and new current / addr, step, newcur, olddiff
	+token rot, over, xor, zerolt, swap ; new diff and step have different signs? / addr, newcur, step^olddiff<0, olddiff
	+token two, pick, rat, sub		; diff limit and previous current / addr, newcur, s^d, olddiff, newdiff
	+token xor, zerolt, and_op, qbbranch
	+baddress xploop_1  ; or diffs before and after have different signs / newdiff^olddiff < 0
	+token drop, drop, rdrop, exit	; exit the loop (leaveaddr on the rstack)
xploop_1:
	+token tor, peek, tor, exit		; continue the loop


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

+header ~cellplus, ~cellplus_n, xploop, xploop_n, 5, "CELL+"
	+forth
	+token twoplus, exit
			
+header ~cells, ~cells_n, cellplus, cellplus_n, 5, "CELLS"
	+forth
	+token dup, add, exit
			
+header ~charplus, ~charplus_n, cells, cells_n, 5, "CHAR+"
	+forth
	+token oneplus, exit
			
+header ~chars, ~chars_n, charplus, charplus_n, 5, "CHARS"
	+forth
	+token exit	; that's correct, just do nothing

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

+header ~rfrom, ~rfrom_n, chars, chars_n, 2, "R>"
	+code
	+rpop
	+dpush
	jmp next

+header ~tor, ~tor_n, rfrom, rfrom_n, 2, ">R"
	+code
	+dpop
	+rpush
	jmp next

+header ~rat, ~rat_n, tor, tor_n, 2, "R@"
	+code
	ldy #3
	lda (_rstack),y
	tax
	dey
	lda (_rstack),y
	+dpush
	jmp next

+header ~rdrop, ~rdrop_n, rat, rat_n, 5, "RDROP"
	+code
	+rpop
	jmp next

+header ~twotor, ~twotor_n, rdrop, rdrop_n, 3, "2>R"
	+code
	+dpop
	+stax _rscratch
	+dpop
	+rpush
	+ldax _rscratch
	+rpush
	jmp next

+header ~tworfrom, ~tworfrom_n, twotor, twotor_n, 3, "2R>"
	+code
	+rpop
	+stax _rscratch
	+rpop
	+dpush
	+ldax _rscratch
	+dpush
	jmp next

+header ~tworat, ~tworat_n, tworfrom, tworfrom_n, 3, "2R@"
	+code
	ldy #5
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

+header ~peek, ~peek_n, tworat, tworat_n, 1, "@"
	+code
	ldy #1
	lda (_dtop),y
	tax
	dey
	lda (_dtop),y
	+stax _dtop
	jmp next

+header ~cpeek, ~cpeek_n, peek, peek_n, 2, "C@"
	+code
	ldy #0
	lda (_dtop),y
	sta _dtop
	sty _dtop+1
	jmp next

+header ~poke, ~poke_n, cpeek, cpeek_n, 1, "!"
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

+header ~cpoke, ~cpoke_n, poke, poke_n, 2, "C!"
	+code
	+dpop
	+stax _wscratch
	+dpop
	ldy #0
	sta (_wscratch),y
	jmp next

+header ~twopoke, ~twopoke_n, cpoke, cpoke_n, 2, "2!"
	+forth
	+token swap, over, poke, cellplus, poke, exit

+header ~twopeek, ~twopeek_n, twopoke, twopoke_n, 2, "2@"
	+forth
	+token dup, cellplus, peek, swap, peek, exit

; ==============================================================================
; Literal. One of the most common words to see in the compiled code - will take the next parameter and
; push it to stack

;
; code lit
; Alternative but slower: : lit r@ peek r> cell+ >r ; nonstandard
;

+header ~lit, ~lit_n, twopeek, twopeek_n, 3, "LIT"
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

; a lot of LIT calls are for small values
+header ~blit, ~blit_n, lit, lit_n
	+code
	ldy #0
	lda (_ri),y
	ldx #0
	+dpush
	inc _ri
	bne blit_1
	inc _ri+1
blit_1:
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

+header ~base, ~base_n, blit, blit_n, 4, "BASE"
	+code doconst
	+value _base

+header ~bhash, ~bhash_n, base, base_n, 2, "<#"
	+forth
	+token lit
	+value _hldend
	+token lit
	+value _hld
	+token poke, exit

+header ~hash, ~hash_n, bhash, bhash_n, 1, "#"
	+forth
	+token base, peek, udmod, rot, blit
	+bvalue '0'
	+token add
	+token dup, blit
	+bvalue '9'
	+token greater, qbbranch
	+baddress hash_1
	+token blit
	+bvalue 7
	+token add
hash_1:
	+token hold, exit

+header ~hashs, ~hashs_n, hash, hash_n, 2, "#S"
	+forth
hashs_1:
	+token hash, twodup, or, zeroeq, qbranch
	+address hashs_1
	+token exit

+header ~hashb, ~hashb_n, hashs, hashs_n, 2, "#>"
	+forth
	+token twodrop, lit
	+value _hld
	+token peek, lit
	+value _hldend
	+token over, sub, exit

+header ~hold, ~hold_n, hashb, hashb_n, 4, "HOLD"
	+forth
	+token lit
	+value _hld
	+token peek, one, sub, dup, lit
	+value _hld
	+token poke, cpoke, exit

+header ~sign, ~sign_n, hold, hold_n, 4, "SIGN"
	+forth
	+token zerolt, qbbranch
	+baddress sign_1
	+token blit
	+bvalue '-'
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

+header ~ddotr, ~ddotr_n, sign, sign_n, 3, "D.R"
	+forth
	+token tor, dup, tor, dabs, bhash, hashs, rfrom
	+token sign, hashb, rfrom, over, sub, spaces, type, exit

+header ~ddot, ~ddot_n, ddotr, ddotr_n, 2, "D."
	+forth
	+token zero, ddotr, space, exit

+header ~dot, ~dot_n, ddot, ddot_n, 1, "."
	+forth
	+token stod, ddot, exit

;
; : decimal 10 base ! ;
; : hex 16 base ! ;
;

+header ~decimal, ~decimal_n, dot, dot_n, 7, "DECIMAL"
	+forth
	+token blit
	+bvalue 10
	+token base, poke, exit

+header ~hex, ~hex_n, decimal, decimal_n, 3, "HEX"
	+forth
	+token blit
	+bvalue 16
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

+header ~incpoke, ~incpoke_n, hex, hex_n, 2, "+!"
	+forth
	+token dup, peek, rot, add, swap, poke, exit

+header ~here, ~here_n, incpoke, incpoke_n, 4, "HERE"
	+forth
	+token lit
	+value _here
	+token peek, exit

+header ~allot, ~allot_n, here, here_n, 5, "ALLOT"
	+forth
	+token lit
	+value _here
	+token incpoke, exit

+header ~comma, ~comma_n, allot, allot_n, 1, ","
	+forth
	+token here, two, allot, poke, exit

+header ~ccomma, ~ccomma_n, comma, comma_n, 2, "C,"
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

+header ~branch, ~branch_n, ccomma, ccomma_n, 6, "BRANCH"
	+code
branch_c:
	ldy #1
	lda (_ri),y
	tax
	dey
	lda (_ri),y
	+stax _ri
	jmp next

+header ~qbranch, ~qbranch_n, branch, branch_n, 7, "?BRANCH"
	+code
	+dpop
	stx _rscratch
	ora _rscratch
	beq branch_c
	+ldax _ri
	+incax 2
	+stax _ri
	jmp next

; potentially saving a few byte by switching to short relative forward offsets in branches (majority of them)
+header ~bbranch, ~bbranch_n, qbranch, qbranch_n
	+code
bbranch_c:
	ldy #0
	lda (_ri),y
	clc
	adc _ri
	sta _ri
	tya
	adc _ri+1
	sta _ri+1
	jmp next
	
+header ~qbbranch, ~qbbranch_n, bbranch, bbranch_n
	+code
	+dpop
	stx _rscratch
	ora _rscratch
	beq bbranch_c
	inc _ri
	bne qbbranch_1
	inc _ri+1
qbbranch_1:
	jmp next

	
	
; ==============================================================================
; Line input support

+header ~tib, ~tib_n, qbbranch, qbbranch_n, 3, "TIB"
	+forth
	+token lit
	+value _source
	+token peek, twoplus, twoplus, twoplus, peek, exit

+header ~ptrin, ~ptrin_n, tib, tib_n, 3, ">IN"
	+forth
	+token lit
	+value _source
	+token peek, twoplus, twoplus, exit

+header ~numtib, ~numtib_n, ptrin, ptrin_n, 4, "#TIB"
	+forth
	+token lit
	+value _source
	+token peek, twoplus, twoplus, twoplus, twoplus, exit

+header ~source, ~source_n, numtib, numtib_n, 6, "SOURCE"
	+forth
	+token tib, numtib, peek, exit

+header ~sourceid, ~sourceid_n, source, source_n, 9, "SOURCE-ID"
	+forth
	+token lit
	+value _source
	+token peek, twoplus, peek, exit

+header ~accept, ~accept_n, sourceid, sourceid_n, 6, "ACCEPT"
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

; : refill source-id 0< if false exit then
;          source-id 0= if cr ." Ok" cr tib 80 accept #tib ! 0 >in ! true exit then
;          source-id file-position drop _source 10 + 2!
;          tib 98 source-id read-line 0= and
;          if #tib ! 0 >in ! true exit then
;          drop false ;

; Note that slightly longer lines are being read from file
+header ~refill, ~refill_n, accept, accept_n, 6, "REFILL"
	+forth
	+token sourceid, zerolt, qbbranch
	+baddress refill_1
	+token false, exit	; "EVALUATE" - no refill
refill_1:
	+token sourceid, zeroeq, qbbranch
	+baddress refill_2
	+token cr, lit
	+value prompt
	+token count, type, cr, tib, blit
	+bvalue 80
	+token accept, numtib, poke, zero, ptrin, poke, true, exit	; console
refill_2:
	+token sourceid, fileposition, drop, lit
	+value _source
	+token peek, blit
	+bvalue 10
	+token add, twopoke
	+token tib, blit
	+bvalue 98
	+token sourceid, readline, zeroeq, and_op, qbbranch
	+baddress refill_3
	+token numtib, poke, zero, ptrin, poke, true, exit	 ; file (note that the position is saved _before_ the refill)
refill_3:
	+token drop, false, exit
prompt:
	+string 2, "OK"

; ==============================================================================
; Some basic text output

+header ~emit, ~emit_n, refill, refill_n, 4, "EMIT"
	+code
	+dpop
	jsr CHROUT
	jmp next

+header ~cr, ~cr_n, emit, emit_n, 2, "CR"
	+forth
	+token blit
	+bvalue NEW_LINE
	+token emit, exit

+header ~bl, ~bl_n, cr, cr_n, 2, "BL"
	+code doconst
	+value ' '

+header ~space, ~space_n, bl, bl_n, 5, "SPACE"
	+forth
	+token bl, emit, exit

+header ~type, ~type_n, space, space_n, 4, "TYPE"
	+forth
type_1:
	+token qdup, qbbranch
	+baddress type_done
	+token oneminus, swap, dup, cpeek, emit, oneplus, swap, branch
	+address type_1
type_done:
	+token drop, exit

+header ~count, ~count_n, type, type_n, 5, "COUNT"
	+forth
	+token dup, oneplus, swap, cpeek, exit

; ==============================================================================
; Word lookup. This is where the complex things begin.

+header ~word, ~word_n, count, count_n, 4, "WORD"
	+forth
	+token tor, source, swap, ptrin, peek, add
word_1:
	+token over, ptrin, peek, greater, qbbranch
	+baddress word_2
	+token dup, cpeek, blit
	+bvalue 127 ; a workaround for peculiar C64 annoyance
	+token and_op, rat, equal, qbbranch
	+baddress word_2
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

+header ~parse, ~parse_n, word, word_n, 5, "PARSE"
	+forth
	+token tor, source, ptrin, peek, sub
	+token oneplus, tor, ptrin, peek, add, dup, zero
parse_1:
	+token over, cpeek, rfrom, oneminus, qdup, qbbranch
	+baddress parse_3
	+token swap, blit
	+bvalue 127
	+token and_op, rat, equal, qbbranch
	+baddress parse_2 ; SAME AS ABOVE
	+token drop, swap, drop, rdrop, ptrin
	+token dup, peek, oneplus, swap, poke, exit
parse_2:
	+token tor, swap, oneplus, swap, oneplus, ptrin
	+token dup, peek, oneplus, swap, poke, branch
	+address parse_1
parse_3:
	+token drop, swap, drop, rdrop, exit

+header ~parsename, ~parsename_n, parse, parse_n, 10, "PARSE-NAME"
	+forth
	+token source, swap, ptrin, peek, add
parsename_1:
	+token over, ptrin, peek, greater, qbbranch
	+baddress parsename_2
	+token dup, cpeek, bl, equal, qbbranch
	+baddress parsename_2
	+token ptrin, peek, oneplus, ptrin, poke, oneplus, branch
	+address parsename_1
parsename_2:
	+token twodrop, bl, parse, exit

; This is one of the less trivial transitions in the current model. There is a trap here -
; using namemask to isolate just the low 5 bits of the length field is unsafe, it may
; misidentify bytes as length bytes (e.g., "AB" will break it). This means that there is only
; one bit for flags in that field.

+header ~lfatonfa, ~lfatonfa_n, parsename, parsename_n		; known in some dialects as L>NAME, non-standard
	+forth
	+token oneminus, zero
lfatonfa_next:
	+token over, cpeek, blit
	+bvalue $7f
	+token and_op, over, notequal, qbbranch
	+baddress lfatonfa_found
	+token swap, oneminus, swap, oneplus, dup, blit
	+bvalue 31
	+token greater, qbranch
	+address lfatonfa_next
	+token drop, drop, zero, exit
lfatonfa_found:
	+token drop, exit

+header ~tobody, ~tobody_n, lfatonfa, lfatonfa_n, 5, ">BODY"
	+code
	asl _dtop
	rol _dtop+1
	lda #>TOKENS
	adc _dtop+1		; note that C is guaranteed to be 0 here
	sta _dtop+1
	ldy #1
	lda (_dtop),y
	tax
	dey
	lda (_dtop),y
	clc
	adc #3			; offset for JMP CREATED
	sta _dtop
	txa
	adc #0
	sta _dtop+1
	jmp next

; ==============================================================================

+header ~context, ~context_n, tobody, tobody_n		; known in some dialectes as CONTEXT, obsolete with Search-Order
	+forth
	+token blit
	+bvalue _current
	+token peek, tobody, twoplus, exit

+header ~latest, ~latest_n, context, context_n		; known in some dialectes as LATEST, non-standard
	+forth
	+token lit
	+value _latest
	+token peek, exit

; Search-order words

+header ~get_order, ~get_order_n, latest, latest_n, 9, "GET-ORDER"
	+code
	lda _orderitems
	beq +
	sta _scratch
	lda #<_order
	ldx #>_order
	+stax _wscratch
-:
	ldy #1
	lda (_wscratch),y
	tax
	dey
	lda (_wscratch),y
	+dpush
	inc _wscratch
	inc _wscratch
	dec _scratch
	bne -
+:
	lda _orderitems
	ldx #0
	+dpush
	jmp next

+header ~set_order, ~set_order_n, get_order, get_order_n, 9, "SET-ORDER"
	+code
	+dpop
	cmp #255
	bne +
	lda #1
	sta _orderitems
	lda #<(forth_system+1)
	ldx #>(forth_system+1)
	+stax _order
-:
	jmp next
+:
	sta _orderitems
	sta _scratch
	sec
	sbc #1
	asl
	clc
	adc #<_order
	ldx #>_order
	+stax _wscratch
--:
	dec _scratch
	bmi -
	+dpop
	ldy #0
	sta (_wscratch),y
	txa
	iny
	sta (_wscratch),y
	dec _wscratch
	dec _wscratch
	bne --

; Structure of wordlist word
; - header
; - JMP doconst (constant semantics)
; - xt of self, used as wid
; - NULL as there are no items in the list yet

+header ~xwordlist, ~xwordlist_n, set_order, set_order_n
	+forth
	+token xcreate, blit
	+bvalue JMP_INSTR
	+token ccomma, lit
	+value doconst
	+token comma, blit
	+bvalue _hightoken
	+token peek, dup, comma
	+token zero, comma, exit

+header ~search_wordlist, ~search_wordlist_n, xwordlist, xwordlist_n, 15, "SEARCH-WORDLIST"
	+forth
	+token tobody, twoplus, peek
	+token xfind, dup, qbbranch
	+baddress sw_notfound
	+token dup, minusone, swap, cpeek, blit
	+bvalue $80
	+token and_op, qbbranch
	+baddress sw_notimm
	+token negate
sw_notimm:
	+token swap, nfatolfa, lfatocfa, cfatoxt, swap
sw_notfound:
	+token exit

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

+header ~xfind, ~xfind_n, search_wordlist, search_wordlist_n		; caddr, n, start_NFA -> NFA | 0
	+code
	+dpop
	+stax _rscratch
	+dpop
	cmp #0
	beq xfind_nomorewords
	sta _scratch

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
	lda #0
	sta _wscratch+1
	txa					; expect current length in X
	tay
	iny					; name + name byte, now pointing at the LFA
	lda (_rscratch),y
	beq xfind_nomorewords
	bpl xfind_offsetok
	and #$7f
	sta _wscratch+1
	iny
	lda (_rscratch),y
xfind_offsetok:
	sta _wscratch

	sec
	lda _rscratch
	sbc _wscratch
	sta _rscratch
	lda _rscratch+1
	sbc _wscratch+1
	sta _rscratch+1
	
	jmp xfind_compare

xfind_nomorewords:
	sta _dtop
	sta _dtop+1
	jmp next

xfind_found:
	+ldax _rscratch
	+stax _dtop
	jmp next


+header ~lfatocfa, ~lfatocfa_n, xfind, xfind_n
	+code
	ldy #0
	lda (_dtop),y
	bpl +
	iny
+:
	iny
	tya
	clc
	adc _dtop
	sta _dtop
	bcc +
	inc _dtop+1
+:
	jmp next

+header ~nfatolfa, ~nfatolfa_n, lfatocfa, lfatocfa_n
	+code
	ldy #0
	lda (_dtop),y
	and #NAMEMASK
	clc
	adc #1
	adc _dtop
	sta _dtop
	bcc +
	inc _dtop+1
+:
	jmp next
	
+header ~cfatolfa, ~cfatolfa_n, nfatolfa, nfatolfa_n
	+forth
	+token dup, twominus, cpeek, blit
	+bvalue $80
	+token and_op, qbbranch
	+baddress cfatolfa_1
	+token oneminus
cfatolfa_1:
	+token oneminus, exit

+header ~cfatoxt, ~cfatoxt_n, cfatolfa, cfatolfa_n
	+code
	lda #0
	sta _rscratch
	lda #>TOKENS
	sta _rscratch+1
	
x_0:
	ldy #0
	lda _dtop
	cmp (_rscratch),y
	bne x_1
	iny
	lda _dtop+1
	cmp (_rscratch),y
	bne x_1
	lda _rscratch+1
	sec
	sbc #>TOKENS
	lsr
	sta _dtop+1
	lda _rscratch
	ror
	sta _dtop
	jmp next
x_1:
	inc _rscratch
	inc _rscratch
	bne x_0
	inc _rscratch+1
	bne x_0
	
	lda #0
	sta _rscratch
	sta _scratch
	sta _scratch+1
	lda #>TOKENS
	sta _rscratch+1
	
	ldy #0
cfatoxt_next:
	lda _dtop
	cmp (_rscratch),y
	bne cfatoxt_2
	iny
	lda _dtop+1
	cmp (_rscratch),y
	bne cfatoxt_1
	+ldax _scratch
	+stax _dtop
	jmp next
	
cfatoxt_2:
	iny
cfatoxt_1:
	inc _scratch
	bne +
	inc _scratch+1
+:
	bne cfatoxt_next
	inc _rscratch+1
	bne cfatoxt_next
	jmp next



+header ~find, ~find_n, cfatoxt, cfatoxt_n, 4, "FIND"
	+forth
	+token zero, blit
	+bvalue _orderitems
	+token peek, oneminus, twomult	; ( c-addr 0 2x#order )
find_next:
	+token dup, zerolt, qbbranch
	+baddress find_continue
	+token drop, exit
find_continue:
	+token tor, over, count, rat, blit
	+bvalue _order
	+token add, peek
	+token search_wordlist, qdup, qbbranch
	+baddress find_step
	+token twoswap, twodrop, rdrop, exit
find_step:
	+token rfrom, twominus, branch
	+address find_next

+header ~xdigit, ~xdigit_n, find, find_n, 7, "(DIGIT)"
	+code
	lda _dtop
	+sub '0'
	cmp #10
	bmi xdigit_1
	+sub 'A'-'0'-10
	cmp _base
	bpl xdigit_2
	cmp #0
	bpl xdigit_1
xdigit_2:
	lda #255
	sta _dtop+1
xdigit_1:
	sta _dtop
	jmp next

+header ~tonumber, ~tonumber_n, xdigit, xdigit_n, 7, ">NUMBER"
	+forth
tonumber_1:
	+token dup, zerogt, qbbranch
	+baddress tonumber_3									; no more digits left?
	+token over, cpeek, xdigit, dup, zerolt, zeroeq, qbbranch
	+baddress tonumber_2	; not a possible digit?
	+token dup, base, peek, less, qbbranch
	+baddress tonumber_2						; not a digit in current base?
	+token swap, oneminus, tor, swap, oneplus, tor, tor
	+token base, peek, udmult, rfrom, mplus, rfrom, rfrom
	+token branch
	+address tonumber_1												; and repeat
tonumber_2:
	+token drop
tonumber_3:
	+token exit

+header ~number, ~number_n, tonumber, tonumber_n, 6, "NUMBER"
	+forth
	+token count, base, peek, tor
	+token dup, blit
	+bvalue 3
	+token equal, two, pick, cpeek, blit
	+bvalue 39
	+token equal, and_op			; character as 'c'
	+token two, pick, two, add, cpeek, blit
	+bvalue 39
	+token equal, and_op, qbbranch
	+baddress number_8
	+token drop, oneplus, cpeek, bbranch
	+baddress number_5
number_8:
	+token dup, one, greater, qbbranch
	+baddress number_9
	+token over, cpeek, blit
	+bvalue 35
	+token equal, qbbranch
	+baddress number_11
	+token decimal, bbranch
	+baddress number_10
number_11:
	+token over, cpeek, blit
	+bvalue 36
	+token equal, qbbranch
	+baddress number_12
	+token hex, bbranch
	+baddress number_10
number_12:
	+token over, cpeek, blit
	+bvalue 37
	+token equal, qbbranch
	+baddress number_9
	+token two, base, poke
number_10:
	+token swap, oneplus, swap, oneminus
number_9:
	+token twodup, false, tor, over, cpeek, blit
	+bvalue 45
	+token equal, qbbranch
	+baddress number_1
	+token rdrop, true, tor, oneminus, swap, oneplus, swap
number_1:
	+token zero, dup, twoswap, tonumber, qdup, qbbranch
	+baddress number_4
	+token one, equal, swap, cpeek, blit
	+bvalue 46
	+token equal, and_op, qbbranch
	+baddress number_7	; one unconverted char and it's '.'?
	+token rfrom, qbbranch
	+baddress number_2
	+token dnegate
number_2:
	+token twoswap, twodrop, state, peek, qbbranch
	+baddress number_3
	+token compile, lit, swap
	+token comma, compile, lit, comma
number_3:
	+token bbranch
	+baddress number_6
number_4:
	+token drop, twoswap, twodrop, drop, rfrom, qbbranch
	+baddress number_5
	+token negate
number_5:
	+token state, peek, qbbranch
	+baddress number_6
	+token compile, lit, comma
number_6:
	+token rfrom, base, poke, exit
number_7:
	+token twodrop, type, xabortq
	+string 2, " ?"


; ==============================================================================
; Nice service word that prints the entire list of supported words.

; TODO - make it reusable in other places
+header ~nextword, ~nextword_n, number, number_n ; CURRENT, LFA -> PREV
	+code
	ldy #0
	sty _scratch+1
	lda (_dtop),y
	beq nextword_done
nextword_1:
	bpl nextword_2
	and #$7F
	sta _scratch+1
	iny
	lda (_dtop),y
nextword_2:
	sta _scratch
	+dpop
	sec
	lda _dtop
	sbc _scratch
	sta _dtop
	lda _dtop+1
	sbc _scratch+1
	sta _dtop+1
nextword_x:
	jmp next
nextword_done:
	+dpop
	lda #0
	sta _dtop
	sta _dtop+1
	beq nextword_x

; ==============================================================================
; Outer interpreter

+header ~state, ~state_n, nextword, nextword_n, 5, "STATE"
	+code doconst
	+value _state

+header ~qcomp, ~qcomp_n, state, state_n,  5, "?COMP"
	+forth
	+token state, peek, qbbranch
	+baddress qcomp_badstate
	+token exit
qcomp_badstate:
	+token xabortq
	+string 5, "?COMP"

+header ~qstack, ~qstack_n, qcomp, qcomp_n, 6, "?STACK"
	+forth
	+token depth, dup, zerolt, swap, lit
	+value STACKLIMIT
	+token greater, or, qbbranch
	+baddress qstack_1
	+token xabortq
	+string 6, "?STACK"
qstack_1:
	+token exit

+header ~interpret, ~interpret_n, qstack, qstack_n, 9, "INTERPRET"
	+forth
interpret_1:
	+token qstack, bl, word, dup, cpeek, qbbranch
	+baddress interpret_done	; get the next word if any
	+token state, peek, qbbranch
	+baddress interpret_int
	+token find, qdup, qbbranch
	+baddress comp_num
	+token zerolt, qbbranch
	+baddress comp_imm		; compiling now
	+token compilecomma, branch
	+address interpret_1		; regular word in compile mode
comp_imm:
	+token execute, branch
	+address interpret_1		; immediate word in compile mode
comp_num:
	+token number, branch
	+address interpret_1
interpret_int:
	+token find, qbbranch
	+baddress int_num			; interpreting now
	+token execute, branch
	+address interpret_1		; any word in interpreter mode
int_num:
	+token number, branch
	+address interpret_1
interpret_done:
	+token drop, refill, zeroeq, qbranch
	+address interpret_1
	+token closesource, exit

+header ~closesource, ~closesource_n, interpret, interpret_n, 12, "CLOSE-SOURCE"
	+forth
	+token sourceid, qbbranch
	+baddress closesource_2						; nothing to do with console source
	+token sourceid, zerogt, qbbranch
	+baddress closesource_1
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

; ==============================================================================
; Colon definition and related words
; (CREATE) takes cstr,n and creates a raw header (NFA+LFA)
+header ~xcreate, ~xcreate_n, closesource, closesource_n
	+code

; calculate contents for the new LFA
; if CURRENT has not been set yet, this is the first word, needs to be connected
; to FORTH-SYSTEM, otherwise it links into the context
; Temporarily reloading _latest as it will be updated on the next step
	lda _current
	ora _current+1
	beq +
	lda _current
	asl
	sta _rscratch
	lda _current+1
	rol
	adc #>TOKENS
	sta _rscratch+1		; _rscratch has the address of the token ref (so the token CFA)
	clc
	ldy #0
	lda (_rscratch),y
	adc #5
	sta _scratch
	iny
	lda (_rscratch),y
	adc #0
	sta _scratch+1		; _scratch now has the address of the second word in the referenced body
	lda (_scratch),y	; copy that word to latest (it's NFA of the last word in the dictionary)
	sta _latest+1		; and copy HERE to that address (it's NFA of the word being created)
;	lda _here+1
;	sta (_scratch),y
	dey
	lda (_scratch),y
	sta _latest
;	lda _here
;	sta (_scratch),y
+:
	lda _latest			; if _latest is NULL at this point, the LFA should be NULL as well, not a diff (TODO: validate)
	ora _latest+1
	bne +
	sta _wscratch
	sta _wscratch+1
	beq ++
+:
	sec
	lda _here
	sbc _latest
	sta _wscratch
	lda _here+1
	sbc _latest+1
	sta _wscratch+1
++:
	
	+ldax _here		; register the word as LATEST
	+stax _latest
	
	+dpop
	and #NAMEMASK
	ldy #0
	sta (_here),y
	iny
	tax
	beq xcreate_5
	lda _dtop
	bne xcreate_3
	dec _dtop+1
xcreate_3:
	dec _dtop
	
xcreate_1:			; copy name string
	lda (_dtop),y
	sta (_here),y
	iny
	dex
	bne xcreate_1
	
xcreate_5:
	lda _wscratch	; write LFA
	and #$80
	ora _wscratch+1	; >= 128?
	beq xcreate_4
	lda _wscratch+1
	ora #$80
	sta (_here),y
	iny
xcreate_4:
	lda _wscratch
	sta (_here),y
	iny
	
	tya
	clc				; update HERE
	adc _here
	sta _here
	bcc +
	inc _here+1
+:

	; update token table
	inc _hightoken
	bne +
	inc _hightoken+1
+:
	+ldax _hightoken
	asl
	sta _wscratch
	txa
	rol
	adc #>TOKENS
	sta _wscratch+1
	ldy #0
	lda _here
	sta (_wscratch),y
	iny
	lda _here+1
	sta (_wscratch),y

	+dpop
	jmp next

+header ~create, ~create_n, xcreate, xcreate_n, 6, "CREATE"
	+forth
	+token here, bl, word, count, xcreate, blit
	+bvalue JMP_INSTR
	+token ccomma, lit
	+value created
	+token comma, context, poke, exit

; DOES> is a weird beast. It generates code that will modify the execution of the
; last defined word to jump to the definition word. It is also quite non-portable as it generates a low level instruction
+header ~xcode, ~xcode_n, create, create_n, 7, "(;CODE)"
	+forth
	+token rfrom										; which is the address of the "call xdoes" instruction
	+token latest, count, blit
	+bvalue NAMEMASK
	+token and_op, add, lfatocfa ;twoplus	; CFA of the last defined word
	+token oneplus ; PFA (!)
	+token poke, exit										; and this will actually exit the defining word


+header ~qdefer, ~qdefer_n, xcode, xcode_n, 6, "?DEFER"
	+forth
	+token dup, tobody, twominus, peek, lit
	+value dodefer
	+token equal, qbbranch
	+baddress qdefer_1
	+token exit
qdefer_1:
	+token xabortq
	+string 6, "?DEFER"

+header ~deferpeek, ~deferpeek_n, qdefer, qdefer_n, 6, "DEFER@"
	+forth
	+token qdefer, tobody, peek, exit

+header ~deferpoke, ~deferpoke_n, deferpeek, deferpeek_n, 6, "DEFER!"
	+forth
	+token qdefer, tobody, poke, exit

+header ~comppoke, ~comppoke_n, deferpoke, deferpoke_n
	+forth
	+token compile, lit, comma, compile, poke, exit

+header ~tick, ~tick_n, comppoke, comppoke_n, 1, "'"
	+forth
	+token bl, word, find, drop, exit

+header ~btick, ~btick_n, tick, tick_n, 3 + IMM_FLAG, "[']"
	+forth
	+token qcomp, tick
	+token compile, lit, comma
	+token exit

; This will get the next parameter, compile it to the current definition and skip
+header ~compile, ~compile_n, btick, btick_n, 7, "COMPILE"
	+forth
	+token rfrom, dup, oneplus, swap, cpeek, dup
	+token blit
	+bvalue 16
	+token less, qbbranch
	+baddress compile_lastbyte
	+token ccomma, dup, oneplus, swap, cpeek
compile_lastbyte:
	+token ccomma, tor, exit

+header ~compilecomma, ~compilecomma_n, compile, compile_n, 8, "COMPILE,"
	+forth
	+token dup, blit
	+bvalue 255
	+token greater, qbbranch
	+baddress compilecomma_1
	+token dup, blit
	+bvalue 8
	+token rshift, ccomma
compilecomma_1:
	+token ccomma, exit

+header ~bracket, ~bracket_n, compilecomma, compilecomma_n, 1 + IMM_FLAG, "["
	+forth
	+token qcomp, false, state, poke, exit

+header ~bracketx, ~bracketx_n, bracket, bracket_n, 1, "]"
	+forth
	+token true, state, poke, exit

+header ~commaquote, ~commaquote_n, bracketx, bracketx_n, 2, ",\""
	+forth
	+token blit
	+bvalue '"'
	+token parse, dup, ccomma, here
	+token over, allot, swap, cmove, exit

+header ~cquote, ~cquote_n, commaquote, commaquote_n, 2 + IMM_FLAG, "C\""
	+forth
	+token qcomp, compile, branch, fmark
	+token here, swap, commaquote, fresolve, compile, lit, comma
	+token exit


+header ~xabortq, ~xabortq_n, cquote, cquote_n, 8, "(ABORT\")"
	+forth
	+token rat, count, type, abort, exit

+header ~xquit, ~xquit_n, xabortq, xabortq_n
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


+header ~xforget, ~xforget_n, xquit, xquit_n	; xt -> (delete all words from this address)
	+forth
	+token dup, tobody, twominus, oneminus	; now at cfa
	+token dup, twominus, cpeek, blit
	+bvalue $80
	+token and_op, qbbranch
	+baddress xforget_1
	+token oneminus
xforget_1:
	+token oneminus, dup					; now at lfa
	+token lfatonfa, dup, lit
	+value _here
	+token poke, swap, nextword, dup, lit
	+value _latest
	+token poke, context, poke
	+token oneminus, lit
	+value _hightoken
	+token poke, exit

+header ~addfield, ~addfield_n, xforget, xforget_n, 6, "+FIELD"
	+forth
	+token create, over, comma, add, xcode
	!byte JSR_INSTR
	+address does
	+token peek, add, exit
			

; ==============================================================================
; More control structure support

+header ~fmark, ~fmark_n, addfield, addfield_n, 5, ">MARK"
	+forth
	+token here, zero, comma, exit

+header ~fresolve, ~fresolve_n, fmark, fmark_n, 8, ">RESOLVE"
	+forth
	+token here, swap, poke, exit

+header ~rmark, ~rmark_n, fresolve, fresolve_n, 5, "<MARK"
	+forth
	+token here, exit

+header ~rresolve, ~rresolve_n, rmark, rmark_n, 8, "<RESOLVE"
	+forth
	+token comma, exit

; ==============================================================================
; Some nice to have words

+header ~spaces, ~spaces_n, rresolve, rresolve_n, 6, "SPACES"
	+forth
spaces_1:
	+token dup, zerogt, qbbranch
	+baddress spaces_2
	+token oneminus, space, branch
	+address spaces_1
spaces_2:
	+token drop, exit

; In optional String word set
+header ~cmove, ~cmove_n, spaces, spaces_n, 5, "CMOVE"
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
+header ~cmovex, ~cmovex_n, cmove, cmove_n, 6, "CMOVE>"
	+code
	+dpop
	+stax _scratch
	+dpop
	+stax _wscratch
	+dpop
	+stax _rscratch
	
	jsr moveup
	jmp next

+header ~move, ~move_n, cmovex, cmovex_n, 4, "MOVE"
	+forth
	+token rot, rot, twodup, less, qbbranch
	+baddress move_1
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

+header ~smove, ~smove_n, move, move_n, 5, "SMOVE"
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

+header ~fill, ~fill_n, smove, smove_n, 4, "FILL"
	+forth
	+token swap, tor, swap
fill_1:
	+token rfrom, qdup, qbbranch
	+baddress fill_2
	+token oneminus, tor, twodup, cpoke, oneplus, branch
	+address fill_1
fill_2:
	+token drop, drop, exit


; The next two are non-standard but proposed for inclusion
+header ~place, ~place_n, fill, fill_n, 5, "PLACE"
	+forth
	+token twodup, twotor, charplus, swap
	+token chars, move, tworfrom, cpoke, exit

+header ~plusplace, ~plusplace_n, place, place_n, 6, "+PLACE"
	+forth
	+token dup, count, add, tor, twodup, cpeek
	+token add, swap, cpoke, rfrom, swap, move, exit

; ==============================================================================
; More words from the optional Double-Number word set

wlow = _scratch
whigh = _rscratch

+header ~dadd, ~dadd_n, plusplace, plusplace_n, 2, "D+"
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


+header ~dlit, ~dlit_n, dadd, dadd_n, 4, "2LIT"
	+forth
	+token rat, twopeek, rfrom, twoplus, twoplus, tor, exit


+header ~compdpoke, ~compdpoke_n, dlit, dlit_n
	+forth
	+token compile, lit, comma, compile, twopoke, exit


;
; M*/ is an unusual word that uses three-cell numbers. It is possible to build it from the existing words
; To make it more clear, using some internal helpers:
; : t* ( ud,u -- ut) 2>r r@ m* 0 2r> m* d+ ;
; : t/ ( ut,u -- ud) dup >r um/mod r> swap >r um/mod nip r> ;
; : normsign ( d,n -- ud,u,n ) 2dup xor >r abs rot rot dabs rot r> ;
;
+header ~tmult, ~tmult_n, compdpoke, compdpoke_n
	+forth
	+token twotor, rat, ummult, zero, tworfrom, ummult, dadd, exit

+header ~tdiv, ~tdiv_n, tmult, tmult_n
	+forth
	+token dup, tor, ummod, rfrom, swap
	+token tor, ummod, nip, rfrom, exit

+header ~normsign, ~normsign_n, tdiv, tdiv_n
	+forth
	+token twodup, xor, tor, abs
	+token rot, rot, dabs, rot, rfrom, exit

; ==============================================================================
; Optional File-Access word set

; Forth standard makes assumptions about I/O capabilities that are simply not true
; for most 8-bit systems. Implementing as much as possible to get the system going

_devnum = _rscratch
_secondary = _wscratch

; Full equivalent to C64 OPEN, not exposed to dictionary yet
; Note that it requires 5 stack parameters as the string is enchoded as (c_addr,u)
; Top of data stack will have filenum or 0 on error
+header ~c64open, ~c64open_n, normsign, normsign_n
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
+header ~c64close, ~c64close_n, c64open, c64open_n
	+code
	+dpop
	jsr CLOSE
	jmp next

+header ~ro, ~ro_n, c64close, c64close_n, 3, "R/O"
	+code doconst
	+value ro_v
ro_v:
	+string 4, ",S,R"

+header ~openfile, ~openfile_n, ro, ro_n, 9, "OPEN-FILE"
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
	+token dup, qbbranch
	+baddress of_2
	+token blit
	+bvalue 8
	+token over, lit
	+value _fnamebuf
	+token count, c64open, dup, lit
	+value _openfiles
	+token setbit, dup, zeroeq
of_2:
	+token exit
of_1:
	+string 3, "O0:"

+header ~closefile, ~closefile_n, openfile, openfile_n, 10, "CLOSE-FILE"
	+forth
	+token dup, lit
	+value _openfiles
	+token clearbit, c64close, zero, exit

; Cannot be implemented on C64
+header ~repositionfile, ~repositionfile_n, closefile, closefile_n, 15, "REPOSITION-FILE"
	+forth
	+token drop, twodrop, minusone, exit

; Cannot be implemented on C64
+header ~fileposition, ~fileposition_n, repositionfile, repositionfile_n, 13, "FILE-POSITION"
	+forth
	+token drop, zero, zero, minusone, exit

; A simplistic implementation to test for existence
+header ~filestatus, ~filestatus_n, fileposition, fileposition_n, 11, "FILE-STATUS"
	+forth
	+token ro, openfile, qbbranch
	+baddress filestatus_1
	+token true, exit
filestatus_1:
	+token closefile, zero, exit

+header ~setread, ~setread_n, filestatus, filestatus_n
	+code
	+dpop
	tax
	jsr CHKIN
	jmp next

+header ~xreadchar, ~xreadchar_n, setread, setread_n
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

+header ~xreadbyte, ~xreadbyte_n, xreadchar, xreadchar_n
	+code
	jsr CHRIN
	ldx #0
	+dpush
	jmp next

+header ~iseof, ~iseof_n, xreadbyte, xreadbyte_n
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

+header ~readline, ~readline_n, iseof, iseof_n, 9, "READ-LINE"
	+forth
	+token setread, swap, dup, rot, add, over					; c-addr, c-addr-limit, current
readline_1:
	+token twodup, swap, uless, qbbranch
	+baddress readline_4				; buffer full?
readline_7:
	+token iseof, zeroeq, qbbranch
	+baddress readline_2
	+token xreadchar, qdup, qbranch
	+address readline_7 ; EOF workaround
	+token dup, blit
	+bvalue NEW_LINE
	+token notequal, qbbranch
	+baddress readline_3		; end of line
	+token over, cpoke, oneplus, branch
	+address readline_1 
readline_2:
	+token swap, drop, swap, sub, dup, zeroeq, qbbranch
	+baddress readline_5
	+token false, bbranch
	+baddress readline_6
readline_3:
	+token drop
readline_4:
	+token swap, drop, swap, sub
readline_5:
	+token true
readline_6:
	+token zero, setread, zero, exit

+header ~setwrite, ~setwrite_n, readline, readline_n
	+code
	+dpop
	tax
	jsr CHKOUT
	jmp next

xputchar = emit

; This can be implemented using KERNAL SAVE, but the corresponding READ-LINE cannot be implemented
; with KERNAL LOAD. Leaving it as is for now.
+header ~writefile, ~writefile_n, setwrite, setwrite_n, 10, "WRITE-FILE"
	+forth
	+token setwrite
writefile_1:
	+token qdup, qbbranch
	+baddress writefile_2
	+token swap, dup, cpeek, xputchar, oneplus, swap, oneminus, branch
	+address writefile_1
writefile_2:
	+token drop, zero, setwrite, zero, exit

+header ~includefile, ~includefile_n, writefile, writefile_n, 12, "INCLUDE-FILE"
	+forth
	+token lit
	+value _ibufcount
	+token peek, blit
	+bvalue 7
	+token greater, qbbranch
	+baddress includefile_1
	+token xabortq
	+string 8, "?INCLUDE"
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
	+token peek, blit
	+bvalue 100
	+token mult, add, over, poke
	+token twominus, zero, over, poke
	+token twominus, swap, over, poke
	+token twominus, blit
	+bvalue 6
	+token over, poke
	+token lit
	+value _source
	+token poke
	+token lit
	+value _ibufcount
	+token dup, peek, oneplus, swap, poke
	+token state, peek, qbbranch
	+baddress includefile_2
	+token interpret
includefile_2:
	+token exit

+header ~included, ~included_n, includefile, includefile_n, 8, "INCLUDED"
	+forth
	+token twodup, filestatus, nip, qbbranch
	+baddress included_1
	+token twodrop, exit
included_1:
	+token twodup, here, tor, xcreate, blit	; create a dummy word with the same name as the included file
	+bvalue RTS_INSTR
	+token ccomma, compile, exit, rfrom, context, poke
	+token ro, openfile, qbbranch
	+baddress included_2
	+token drop, exit
included_2:
	+token includefile, exit

+header ~required, ~required_n, included, included_n, 8, "REQUIRED"
	+forth
	+token twodup, context, peek, xfind, qbbranch
	+baddress required_1
	+token twodrop, exit
required_1:
	+token included, exit

; ============================================================================
; Here lies an important boundary - all words above it are used in other core
; words, everythign below is unreferenced. The order is important, so smaller
; token values will fit in one byte making core smaller.
; The boundary is not very precise, there is some slack. Placing words too
; low may cause compile errors, placing too high is benign.
; ============================================================================

+header ~bin, ~bin_n, required, required_n, 3, "BIN"
	+forth
	+token exit		; taking the recommendation and handling all files as binary

+header ~wo, ~wo_n, bin, bin_n, 3, "W/O"
	+code doconst
	+value wo_v
wo_v:
	+string 4, ",S,W"

; This may not be supported on C64, making it identical to W/O
+header ~rw, ~rw_n, wo, wo_n, 3, "R/W"
	+code doconst
	+value wo_v

; For C64 OPEN-FILE and CREATE-FILE are identical
+header ~createfile, ~createfile_n, rw, rw_n, 11, "CREATE-FILE"
	+forth
	+token openfile, exit

; C64 equivalent: OPEN 1,8,15,"S0:Name":CLOSE 1
+header ~deletefile, ~deletefile_n, createfile, createfile_n, 11, "DELETE-FILE"
	+forth
	+token lit
	+value df_1
	+token count, lit
	+value _fnamebuf
	+token place, lit
	+value _fnamebuf
	+token plusplace
	+token one, blit
	+bvalue 8
	+token blit
	+bvalue 15
	+token lit
	+value _fnamebuf
	+token count, c64open
	+token one, notequal, one, c64close, exit
df_1:
	+string 3, "S0:"

; C64 equivalent: OPEN 1,8,15,"R0:NewName=OldName":CLOSE 1
+header ~renamefile, ~renamefile_n, deletefile, deletefile_n, 11, "RENAME-FILE"	; Note that this is the only word that uses PAD
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
	+token one, blit
	+bvalue 8
	+token blit
	+bvalue 15
	+token lit
	+value _fnamebuf
	+token count, c64open
	+token one, notequal, one, c64close, exit
rf_1:
	+string 3, "R0:"
rf_2:
	+string 1, "="

; Cannot be implemented on C64
+header ~resizefile, ~resizefile_n, renamefile, renamefile_n, 11, "RESIZE-FILE"
	+forth
	+token drop, twodrop, minusone, exit

; Cannot be implemented on C64
+header ~filesize, ~filesize_n, resizefile, resizefile_n, 9, "FILE-SIZE"
	+forth
	+token drop, zero, zero, minusone, exit

+header ~readfile, ~readfile_n, filesize, filesize_n, 9, "READ-FILE"
	+forth
	+token setread, swap, dup, rot, add, over					; c-addr, c-addr-limit, current
readfile_1:
	+token twodup, swap, uless, qbbranch
	+baddress readfile_3				; buffer full?
	+token iseof, zeroeq, qbbranch
	+baddress readfile_2
	+token xreadbyte		; end of file?
	+token over, cpoke, oneplus, branch
	+address readfile_1 
readfile_2:
readfile_3:
	+token swap, drop, swap, sub, zero, zero, setread, exit

+header ~writeline, ~writeline_n, readfile, readfile_n, 10, "WRITE-LINE"
	+forth
	+token dup, tor, writefile, rfrom, setwrite, blit
	+bvalue NEW_LINE
	+token xputchar, zero, setwrite, exit

; Not needed on C64
+header ~flushfile, ~flushfile_n, writeline, writeline_n, 10, "FLUSH-FILE"
	+forth
	+token zero, exit

+header ~include, ~include_n, flushfile, flushfile_n, 7, "INCLUDE"
	+forth
	+token parsename, included, exit

+header ~require, ~require_n, include, include_n, 7, "REQUIRE"
	+forth
	+token parsename, required, exit


; ==============================================================================
; Some less commonly used (not used in core) math words
+header ~div, ~div_n, require, require_n, 1, "/"
	+forth
	+token divmod, nip, exit

+header ~mod, ~mod_n, div, div_n, 3, "MOD"
	+forth
	+token divmod, drop, exit

+header ~multdiv, ~multdiv_n, mod, mod_n, 2, "*/"
	+forth
	+token multdivmod, nip, exit

;
; : fm/mod dup >r sm/rem
;          over dup 0<> swap 0< r@ 0< xor and
;          if 1- swap r> + swap else rdrop then ;
;

+header ~fmmod, ~fmmod_n, multdiv, multdiv_n, 6, "FM/MOD"
	+forth
	+token dup, tor, smrem, over, dup, zerone
	+token swap, zerolt, rat, zerolt, xor, and_op, qbbranch
	+baddress fmmod_1
	+token oneminus, swap, rfrom, add, swap, bbranch
	+baddress fmmod_2
fmmod_1:
	+token rdrop
fmmod_2:
	+token exit

+header ~max, ~max_n, fmmod, fmmod_n, 3, "MAX"
	+forth
	+token twodup, less, qbbranch
	+baddress max_1
	+token swap
max_1:
	+token drop, exit

+header ~min, ~min_n, max, max_n, 3, "MIN"
	+forth
	+token twodup, greater, qbbranch
	+baddress min_1
	+token swap
min_1:
	+token drop, exit

;
;	: within over - >r - r> u< ;
;
+header ~within, ~within_n, min, min_n, 6, "WITHIN"
	+forth
	+token over, sub, tor, sub, rfrom, uless, exit

; ==============================================================================
; More words from the optional Double-Number word set

;
; : d= rot = >r = r> and ;
;
+header ~dequal, ~dequal_n, within, within_n, 2, "D="
	+forth
	+token rot, equal, tor, equal, rfrom, and_op, exit

;
; : dmax 2over 2over d< if 2swap then 2drop ;
; : dmin 2over 2over d< invert if 2swap then 2drop ;
;

+header ~dmax, ~dmax_n, dequal, dequal_n, 4, "DMAX"
	+forth
	+token twoover, twoover, dless, qbbranch
	+baddress dmax_1
	+token twoswap
dmax_1:
	+token twodrop, exit

+header ~dmin, ~dmin_n, dmax, dmax_n, 4, "DMIN"
	+forth
	+token twoover, twoover, dless, invert, qbbranch
	+baddress dmin_1
	+token twoswap
dmin_1:
	+token twodrop, exit

;
; : d- dnegate d+ ;
; code d+
;
+header ~dsub, ~dsub_n, dmin, dmin_n, 2, "D-"
	+forth
	+token dnegate, dadd, exit

+header ~dtwodiv, ~dtwodiv_n, dsub, dsub_n, 3, "D2/"
	+forth
	+token dup, one, and_op, blit
	+bvalue 15
	+token lshift, swap, twodiv, swap
	+token rot, twodiv, or, swap, exit

;
; : d2* 2dup d+ ;
;

+header ~dtwomul, ~dtwomul_n, dtwodiv, dtwodiv_n, 3, "D2*"
	+forth
	+token twodup, dadd, exit

+header ~duless, ~duless_n, dtwomul, dtwomul_n, 3, "DU<"
	+forth
	+token rot, twodup, equal, qbbranch
	+baddress duless_1
	+token twodrop, uless, exit
duless_1:
	+token ugreater, qbbranch
	+baddress duless_2
	+token twodrop, true, exit
duless_2:
	+token twodrop, false, exit

;
; : d0= or 0= ;
; : d0< nip 0< ;
; : d< rot > if 2drop true else < then ;
;
+header ~dzeroeq, ~dzeroeq_n, duless, duless_n, 3, "D0="
	+forth
	+token or, zeroeq, exit

+header ~dzeroless, ~dzeroless_n, dzeroeq, dzeroeq_n, 3, "D0<"
	+forth
	+token nip, zerolt, exit

+header ~dless, ~dless_n, dzeroless, dzeroless_n, 2, "D<"
	+forth
	+token rot, twodup, equal, qbbranch
	+baddress dless_1
	+token twodrop, uless, exit
dless_1:
	+token greater, qbbranch
	+baddress dless_2
	+token twodrop, true, exit
dless_2:
	+token twodrop, false, exit

;
; : d>s drop ;
;
+header ~dtos, ~dtos_n, dless, dless_n, 3, "D>S"
	+forth
	+token drop, exit

;
; : 2constant create , , does> 2@ ;
;

+header ~dconstant, ~dconstant_n, dtos, dtos_n, 9, "2CONSTANT"
	+forth
	+token create, comma, comma, xcode
	!byte JSR_INSTR
	+address does
	+token twopeek, exit

;
; : 2lit r@ 2@ r> 2+ 2+ >r ; nonstandard
; : 2literal ?comp state @ if compile 2lit , , then ; immediate
;

+header ~dliteral, ~dliteral_n, dconstant, dconstant_n, 8 + IMM_FLAG, "2LITERAL"
	+forth
	+token qcomp, state, peek, qbbranch
	+baddress dliteral_1
	+token compile, dlit
	+token comma, comma
dliteral_1:
	+token exit

;
; : 2rot 5 roll 5 roll ;
;
+header ~drot, ~drot_n, dliteral, dliteral_n, 4, "2ROT"
	+forth
	+token blit
	+bvalue 5
	+token roll, blit
	+bvalue 5
	+token roll, exit

+header ~dvalue, ~dvalue_n, drot, drot_n, 6, "2VALUE"
	+forth
	+token create, lit
	+value dovalue
	+token here, twominus, poke, lit
	+value dvalue_sem
	+token comma, comma, comma, exit
dvalue_sem:
	+value twopeek
	+value twopoke
	+value compdpoke

;
; : m*/ >r normsign r> swap >r >r t* r> t/ r> 0< if dnegate then ;
;

+header ~mmuldiv, ~mmuldiv_n, dvalue, dvalue_n, 3, "M*/"
	+forth
	+token tor, normsign, rfrom, swap, tor, tor
	+token tmult, rfrom, tdiv, rfrom, zerolt, qbbranch
	+baddress mmuldiv_1
	+token dnegate
mmuldiv_1:
	+token exit


; ==============================================================================
; Reset return stack, dispose of sources, close all open files, and reenter the system.
+header ~quit, ~quit_n, mmuldiv, mmuldiv_n, 4, "QUIT"
	+forth
	+token xsst
	+token xquit ; this is an equivalent to ;CODE
	
; ==============================================================================
+header ~immediate, ~immediate_n, quit, quit_n, 9, "IMMEDIATE"
	+forth
	+token latest, dup, cpeek, blit
	+bvalue IMM_FLAG
	+token or, swap, cpoke, exit

; Note that while DOES> looks like high-level word its implementation is depended on the opcode for native CALL/JSR
+header ~doesx, ~doesx_n, immediate, immediate_n, 5 + IMM_FLAG, "DOES>"
	+forth
	+token qcomp, compile, xcode, blit
	+bvalue JSR_INSTR
	+token ccomma, lit
	+value does
	+token comma, exit	; compile (;CODE) followed by "call does_c"

; Note that colon will remove the word from the search order (to be restored by semicolon)
+header ~colon, ~colon_n, doesx, doesx_n, 1, ":"
	+forth
	+token bl, word, count
	+token xcreate
	+token blit
	+bvalue RTS_INSTR
	+token ccomma
	+token bracketx, exit

; Words defined with :NONAME technically don't need to be linked in the vocabulary but if it is done that way RECURSE becomes harder
; to implement. It is easier just to link the word with emtpy name. In this implementation it has an unusual side effect that FIND
; will actually find the last :NONAME if searched for empty string and the test suite actually traps that (not an error though). But -
; standard does not specify it either way; and this is potentially useful.

;
; : :noname here 0 , latest , _latest ! here ' call , ] ;
;
 
+header ~colonnoname, ~colonnoname_n, colon, colon_n, 7, ":NONAME"
	+forth
	+token zero, dup
	+token xcreate
	+token blit
	+bvalue RTS_INSTR
	+token ccomma
	+token bracketx, lit
	+value _hightoken
	+token peek, exit

+header ~bufferc, ~bufferc_n, colonnoname, colonnoname_n, 7, "BUFFER:"
	+forth
	+token create, allot, exit

+header ~semicolon, ~semicolon_n, bufferc, bufferc_n, 1 + IMM_FLAG, ";"
	+forth
	+token qcomp, compile, exit, bracket, latest, context, poke, exit

+header ~variable, ~variable_n, semicolon, semicolon_n, 8, "VARIABLE"
	+forth
	+token create, zero, comma, exit

+header ~twovariable, ~twovariable_n, variable, variable_n, 9, "2VARIABLE"
	+forth
	+token create, zero, dup, comma, comma, exit

+header ~constant, ~constant_n, twovariable, twovariable_n, 8, "CONSTANT"
	+forth
	+token create, comma, xcode
	!byte JSR_INSTR
	+address does
	+token peek, exit

+header ~defer, ~defer_n, constant, constant_n, 5, "DEFER"
	+forth
	+token create, lit
	+value dodefer
	+token here, twominus, poke, lit	; note that we cannot use "compile exit" here as that will reserve only one byte,
	+value exit							; and some tokens may need two
	+token comma, exit

+header ~actionof, ~actionof_n, defer, defer_n, 9 + IMM_FLAG, "ACTION-OF"
	+forth
	+token state, peek, qbbranch
	+baddress actionof_1
	+token btick, compile, deferpeek, exit
actionof_1:
	+token tick, deferpeek, exit

+header ~is, ~is_n, actionof, actionof_n, 2 + IMM_FLAG, "IS"
	+forth
	+token state, peek, qbbranch
	+baddress is_1
	+token btick, compile, deferpoke, exit
is_1:
	+token tick, deferpoke, exit

; "value" has a special structure: three tokens for read semantics,
; write semantics, and compile semantics, followed by the value itself

+header ~value, ~value_n, is, is_n, 5, "VALUE"
	+forth
	+token create, lit
	+value dovalue
	+token here, twominus, poke, lit
	+value value_sem
	+token comma, comma, exit
value_sem:
	; Note that the parameter block uses "value" instead of "token" - this is
	; intentional as the size of token is not known
	+value peek
	+value poke
	+value comppoke

+header ~to, ~to_n, value, value_n, 2 + IMM_FLAG, "TO"
	+forth
	+token bl, word, find, qbbranch
	+baddress to_2
	+token tobody, dup, twominus, peek, lit
	+value dovalue
	+token equal, qbbranch
	+baddress to_2
	+token dup, twoplus, swap, peek, state, peek, qbbranch
	+baddress to_1
	+token twoplus
to_1:
	+token twoplus, peek, execute, exit
to_2:
	+token xabortq
	+string 6, "?VALUE"

+header ~squote, ~squote_n, to, to_n, 2 + IMM_FLAG, "S\""
	+forth
	+token state, peek, qbbranch
	+baddress squote_1
	+token cquote, compile, count, exit
squote_1:
	+token blit
	+bvalue '"'
	+token parse, lit
	+value _sbuf
	+token lit
	+value _sflip
	+token peek, blit
	+bvalue 100
	+token mult, add, swap, twotor, tworat, cmove
	+token tworfrom, lit
	+value _sflip
	+token dup, peek, one, xor, swap, poke, exit 

+header ~ssquote, ~ssquote_n, squote, squote_n, 3 + IMM_FLAG, "S\\\""
	+forth
	+token tib, ptrin, peek, add, lit
	+value _sbuf
	+token lit
	+value _sflip
	+token peek, blit
	+bvalue 100
	+token mult, add, numtib, peek
	+token ptrin, peek, sub, over, tor
	+token smove, swap, ptrin, incpoke
	+token rfrom, swap, lit
	+value _sflip
	+token dup, peek, one, xor, swap, poke
	+token state, peek, qbbranch
	+baddress ssquote_1
	+token compile, branch, fmark
	+token here, two, pick, dup, ccomma, allot, swap, fresolve
	+token compile, lit, dup
	+token comma, compile, count, oneplus, swap, cmove 
ssquote_1:
	+token exit

+header ~dotquote, ~dotquote_n, ssquote, ssquote_n, 2 + IMM_FLAG, ".\""
	+forth
	+token qcomp, cquote, compile, count, compile, type, exit

+header ~char, ~char_n, dotquote, dotquote_n, 4, "CHAR"
	+forth
	+token bl, word, charplus, cpeek, exit

+header ~bcharb, ~bcharb_n, char, char_n, 6 + IMM_FLAG, "[CHAR]"
	+forth
	+token compile, blit, bl
	+token word, charplus, cpeek, ccomma, exit

+header ~abortq, ~abortq_n, bcharb, bcharb_n, 6 + IMM_FLAG, "ABORT\""
	+forth
	+token qcomp, compile, qbranch, fmark
	+token compile, xabortq, commaquote, fresolve, exit

; In optional Programming-Tools word set
+header ~forget, ~forget_n, abortq, abortq_n, 6, "FORGET"
	+forth
	+token bl, word, find, qbbranch
	+baddress forget_1
	+token xforget
forget_1:
	+token exit

+header ~marker, ~marker_n, forget, forget_n, 6, "MARKER"
	+forth
	+token create, lit
	+value _hightoken
	+token peek, comma, xcode
	!byte JSR_INSTR
	+address does
	+token peek, xforget, exit

+header ~recurse, ~recurse_n, marker, marker_n, 7 + IMM_FLAG, "RECURSE"
	+forth
	+token qcomp, lit
	+value _hightoken
	+token peek, compilecomma, exit

+header ~bcompile, ~bcompile_n, recurse, recurse_n, 9 + IMM_FLAG, "[COMPILE]"
	+forth
	+token qcomp, tick, compilecomma, exit

; Somehow I've managed to get this to pass the tests but I still don't completely understand what
; it is supposed to do
+header ~postpone, ~postpone_n, bcompile, bcompile_n, 8 + IMM_FLAG, "POSTPONE"
	+forth
	+token qcomp, bl, word, find, one, equal, qbbranch
	+baddress postpone_1
	+token compilecomma, exit
postpone_1:
	+token compile, compile, compilecomma, exit

; This word behaves differently depending on compilation state - in compilation it
; will emit LIT followed by the value from the stack
+header ~literal, ~literal_n, postpone, postpone_n, 7 + IMM_FLAG, "LITERAL"
	+forth
	+token qcomp, state, peek, qbbranch
	+baddress literal_1
	+token compile, lit, comma
literal_1:
	+token exit

+header ~tuck, ~tuck_n, literal, literal_n, 4, "TUCK"
	+forth
	+token swap, over, exit

+header ~holds, ~holds_n, tuck, tuck_n, 5, "HOLDS"
	+forth
holds_1:
	+token qdup, qbbranch
	+baddress holds_2
	+token oneminus, twodup, add, cpeek, hold, branch
	+address holds_1
holds_2:
	+token drop, exit

+header ~dotr, ~dotr_n, holds, holds_n, 2, ".R"
	+forth
	+token swap, stod, rot, ddotr, exit

+header ~udot, ~udot_n, dotr, dotr_n, 2, "U."
	+forth
	+token zero, ddot, exit

+header ~udotr, ~udotr_n, udot, udot_n, 3, "U.R"
	+forth
	+token zero, swap, ddotr, exit

+header ~pad, ~pad_n, udotr, udotr_n, 3, "PAD"
	+code doconst
	+value _pad

+header ~unused, ~unused_n, pad, pad_n, 6, "UNUSED"
	+forth
	+token lit
	+value MEMTOP
	+token here, sub, exit

+header ~erase, ~erase_n, unused, unused_n, 5, "ERASE"
	+forth
	+token zero, fill, exit

+header ~sstring, ~sstring_n, erase, erase_n, 7, "/STRING"
	+forth
	+token rot, over, add, rot, rot, sub, exit

+header ~blank, ~blank_n, sstring, sstring_n, 5, "BLANK"
	+forth
	+token bl, fill, exit

+header ~sliteral, ~sliteral_n, blank, blank_n, 8 + IMM_FLAG, "SLITERAL"
	+forth
	+token state, peek, qbbranch
	+baddress sliteral_1
	+token compile, branch, fmark
	+token rot, rot
	+token dup, tor, here, dup, tor
	+token swap, dup, allot, cmove, fresolve
	+token compile, lit, rfrom
	+token comma, compile, lit, rfrom
	+token comma
sliteral_1:
	+token exit

+header ~qmark, ~qmark_n, sliteral, sliteral_n, 1, "?"
	+forth
	+token peek, dot, exit

+header ~dots, ~dots_n, qmark, qmark_n, 2, ".S"
	+forth
	+token depth
dots_1:
	+token qdup, qbbranch
	+baddress dots_2
	+token dup, pick, dot, oneminus, branch
	+address dots_1
dots_2:
	+token exit

+header ~ahead, ~ahead_n, dots, dots_n, 5, "AHEAD"
	+forth
	+token fmark, exit


cstr1 = _dtop
clen1 = _scratch
cstr2 = _rscratch
clen2 = _wscratch

; COMPARE became standard in the later versions of the language.
; In optional String word set
; This one still can be optimized further
+header ~compare, ~compare_n, ahead, ahead_n, 7, "COMPARE" ; (caddr1, u1, caddr2, u2 -> n)
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

; ==============================================================================
;
; : save-input _source @ dup >r @ begin dup while dup 2* r@ + @ swap 1- again drop r> @ ;
;
; : restore-input over source-id = if
;                 source-id 0> if 6 pick 6 pick source-id reposition-file refill 2drop then
;                 begin dup while dup roll over 2* _source @ + ! 1- again drop false
;                 else true then ;
;

+header ~saveinput, ~saveinput_n, compare, compare_n, 10, "SAVE-INPUT"
	+forth
	+token lit
	+value _source
	+token peek, dup, tor, peek
saveinput_1:
	+token qdup, qbbranch
	+baddress saveinput_2
	+token dup, twomult, rat, add, peek, swap, oneminus, branch
	+address saveinput_1
saveinput_2:
	+token rfrom, peek, exit

+header ~restoreinput, ~restoreinput_n, saveinput, saveinput_n, 13, "RESTORE-INPUT"
	+forth
	+token over, sourceid, equal, qbbranch
	+baddress restoreinput_3
	+token sourceid, zerogt, qbbranch
	+baddress restoreinput_1
	+token blit
	+bvalue 6
	+token pick, blit
	+bvalue 6
	+token pick, sourceid, repositionfile, refill, twodrop
restoreinput_1:
	+token qdup, qbbranch
	+baddress restoreinput_2
	+token dup, roll, over, twomult, lit
	+value _source
	+token peek, add, poke, oneminus, branch
	+address restoreinput_1
restoreinput_2:
	+token false, exit
restoreinput_3:
	+token true, exit

+header ~evaluate, ~evaluate_n, restoreinput, restoreinput_n, 8, "EVALUATE"
	+forth
	+token lit
	+value _source
	+token peek
	+token twominus, swap, over, poke
	+token twominus, swap, over, poke
	+token twominus, zero, over, poke
	+token twominus, minusone, over, poke
	+token twominus, blit
	+bvalue 4
	+token over, poke
	+token lit
	+value _source
	+token poke
	+token interpret
	+token exit



; ==============================================================================

+header ~align, ~align_n, evaluate, evaluate_n, 5, "ALIGN"
	+forth
	+token exit

+header ~aligned, ~aligned_n, align, align_n, 7, "ALIGNED"
	+forth
	+token exit


; ==============================================================================
; Control words. All of these are immediate and don't do anything useful
; in interpreter mode. There should be no code calling to CFA of these words.
; To understand the concept behind these words look at the BEGIN/AGAIN pair -
; BEGIN ends up just putting RI on the stack and AGAIN compiles BRANCH to that RI.
; Forward references are a bit trickier but follow the same pattern.

+header ~begin, ~begin_n, aligned, aligned_n, 5 + IMM_FLAG, "BEGIN"
	+forth
	+token qcomp, rmark, exit

+header ~until, ~until_n, begin, begin_n, 5 + IMM_FLAG, "UNTIL"
	+forth
	+token qcomp, compile, qbranch, rresolve
	+token exit

+header ~again, ~again_n, until, until_n, 5 + IMM_FLAG, "AGAIN"
	+forth
	+token qcomp, compile, branch, rresolve
	+token exit

+header ~if, ~if_n, again, again_n, 2 + IMM_FLAG, "IF"
	+forth
	+token qcomp, compile, qbranch, fmark
	+token exit

+header ~then, ~then_n, if, if_n, 4 + IMM_FLAG, "THEN"
	+forth
	+token qcomp, fresolve, exit

+header ~else, ~else_n, then, then_n, 4 + IMM_FLAG, "ELSE"
	+forth
	+token qcomp, compile, branch, fmark
	+token swap, fresolve, exit

+header ~while, ~while_n, else, else_n, 5 + IMM_FLAG, "WHILE"
	+forth
	+token qcomp, compile, qbranch, fmark
	+token swap, exit

+header ~repeat, ~repeat_n, while, while_n, 6 + IMM_FLAG, "REPEAT"
	+forth
	+token qcomp, compile, branch, rresolve
	+token fresolve, exit

+header ~do, ~do_n, repeat, repeat_n, 2 + IMM_FLAG, "DO"
	+forth
	+token qcomp, compile, xdo, fmark, rmark, exit

+header ~qdo, ~qdo_n, do, do_n, 3 + IMM_FLAG, "?DO"
	+forth
	+token qcomp, compile, xqdo, fmark, rmark, exit

+header ~loop, ~loop_n, qdo, qdo_n, 4 + IMM_FLAG, "LOOP"
	+forth
	+token qcomp, compile, xloop, rresolve, fresolve, exit

+header ~ploop, ~ploop_n, loop, loop_n, 5 + IMM_FLAG, "+LOOP"
	+forth
	+token qcomp, compile, xploop, rresolve, fresolve, exit

+header ~i, ~i_n, ploop, ploop_n, 1, "I"
	+forth
	+token rfrom, rat, swap, tor, exit

+header ~j, ~j_n, i, i_n, 1, "J"
	+forth
	+token rfrom, rfrom, rfrom, rfrom, rfrom, dup, tor
	+token swap, tor, swap, tor, swap, tor, swap, tor
	+token exit

+header ~leave, ~leave_n, j, j_n, 5, "LEAVE"
	+forth
	+token rdrop, rdrop, rdrop, exit
			
+header ~unloop, ~unloop_n, leave, leave_n, 6, "UNLOOP"
	+forth
	+token rfrom, rdrop, rdrop, rdrop, tor, exit

+header ~case, ~case_n, unloop, unloop_n, 4 + IMM_FLAG, "CASE"
	+forth
	+token qcomp, depth, rfrom, swap, tor, tor, exit

+header ~of, ~of_n, case, case_n, 2 + IMM_FLAG, "OF"
	+forth
	+token qcomp, compile, over, compile, equal, compile, qbranch
	+token fmark, compile, drop, exit

+header ~endof, ~endof_n, of, of_n, 5 + IMM_FLAG, "ENDOF"
	+forth
	+token qcomp, compile, branch, fmark
	+token swap, fresolve, exit

+header ~endcase, ~endcase_n, endof, endof_n, 7 + IMM_FLAG, "ENDCASE"
	+forth
	+token qcomp, compile, drop, depth
	+token rfrom, rfrom, swap, tor, sub
endcase_1:
	+token qdup, qbbranch
	+baddress endcase_2
	+token oneminus, swap, fresolve, branch
	+address endcase_1
endcase_2:
	+token exit

;
; : ( source-id 0< if
;     begin ')' parse 2drop >in @ #tib @ = tib #tib @ + 1- c@ ')' = and
;     while refill invert if exit then again
;     else ')' parse 2drop then ; immediate
;

+header ~brace, ~brace_n, endcase, endcase_n, 1 + IMM_FLAG, "("
	+forth
	+token sourceid, zerogt, qbbranch
	+baddress brace_2
brace_1:
	+token blit
	+bvalue ')'
	+token parse, twodrop, ptrin, peek, numtib, peek, equal
	+token tib, numtib, peek, add, oneminus, cpeek, blit
	+bvalue ')'
	+token notequal, and_op
	+token qbbranch
	+baddress brace_3
	+token refill, invert, qbranch
	+address brace_1
	+token exit
brace_2:
	+token blit
	+bvalue ')'
	+token parse, twodrop
brace_3:
	+token exit

+header ~backslash, ~backslash_n, brace, brace_n, 1 + IMM_FLAG, "\\"
	+forth
	+token zero, parse, twodrop, exit

+header ~dotbrace, ~dotbrace_n, backslash, backslash_n, 2 + IMM_FLAG, ".("
	+forth
	+token blit
	+bvalue ')'
	+token parse, type, exit


; ==============================================================================
; Small subset from the optional Facility word set

+header ~beginstructure, ~beginstructure_n, dotbrace, dotbrace_n, 15, "BEGIN-STRUCTURE"
	+forth
	+token create, here, zero, zero, comma, xcode
	!byte JSR_INSTR
	+address does
	+token peek, exit

+header ~endstructure, ~endstructure_n, beginstructure, beginstructure_n, 13, "END-STRUCTURE"
	+forth
	+token swap, poke, exit
				
+header ~field, ~field_n, endstructure, endstructure_n, 6, "FIELD:"
	+forth
	+token two, addfield, exit

+header ~cfield, ~cfield_n, field, field_n, 7, "CFIELD:"
	+forth
	+token one, addfield, exit

; ==============================================================================
; Per discussion on forth-standard.org, it appears that this word does not
; have to provide any additional information. Given the overall bad specs and
; high memory use for little purpose, shortwiring it
+header ~environmentq, ~environmentq_n, cfield, cfield_n, 12, "ENVIRONMENT?"
	+code doconst
	+value VAL_FALSE

; In optional Programming-Tools word set
+header ~words, ~words_n, environmentq, environmentq_n, 5, "WORDS"
	+forth
	+token zero, context, peek
words_next:
	+token dup, cpeek, qbbranch
	+baddress words_skip
	+token swap, oneplus, swap, dup, count, blit
	+bvalue NAMEMASK
	+token and_op, type, space
words_skip:
	+token dup, nfatolfa, nextword
	+token dup, zeroeq, qbranch
	+address words_next
	+token drop, cr, dot, lit
	+address words_n
	+token count, type, exit

+header ~key, ~key_n, words, words_n, 3, "KEY"
	+code
	jsr CHRIN			; TODO - this will echo the character, which contradicts the standard
	ldx #0
	+dpush
	jmp next

; ==============================================================================
; This word became standard in ANS Forth, part of optional Programming-Tools word set. Quit the interpreter.
; code bye
+header ~bye, ~bye_n, key, key_n, 3, "BYE"
	+code
	pla
	pla
	rts

; Search-Order words
;

; : also get-order over swap 1+ set-order ;

+header ~also, ~also_n, bye, bye_n, 4, "ALSO"
	+forth
	+token get_order, over, swap, oneplus, set_order, exit


+header ~definitions, ~definitions_n, also, also_n, 11, "DEFINITIONS"
	+code
	lda _orderitems
	asl
	tay
	lda _order-2,y		; Note the back offset by 2
	sta _current
	lda _order-1,y
	sta _current+1
	jmp next

+header ~get_current, ~get_current_n, definitions, definitions_n, 11, "GET-CURRENT"
	+forth
	+token blit
	+bvalue _current
	+token peek, exit

; : only -1 set-order ;

+header ~only, ~only_n, get_current, get_current_n, 4, "ONLY"
	+forth
	+token minusone, set_order, exit

+header ~order, ~order_n, only, only_n, 5, "ORDER"
	+forth
	+token get_order
order_1:
	+token qdup, qbbranch
	+baddress order_2
	+token swap, dup, tobody, twominus, oneminus
	+token cfatolfa, lfatonfa, count, type, blit
	+bvalue '*'
	+token emit
	+token dot, oneminus, branch
	+address order_1
order_2:
	+token exit

; : previous get-order nip 1- set-order ;

+header ~previous, ~previous_n, order, order_n, 8, "PREVIOUS"
	+forth
	+token get_order, nip, oneminus, set_order, exit

+header ~set_current, ~set_current_n, previous, previous_n, 11, "SET-CURRENT"
	+forth
	+token blit
	+bvalue _current
	+token poke, exit

+header ~wordlist, ~wordlist_n, set_current, set_current_n, 8, "WORDLIST"
	+forth
	+token here, zero, dup, xwordlist, swap, context, poke, exit

+header ~forth, ~forth_n, wordlist, wordlist_n, 5, "FORTH"
	+forth
	+token get_order, nip, lit
	+value forth_system+1			; this is the wid of the dynamically created FORTH-WORDLIST
	+token swap, set_order, exit

; ==============================================================================
; The main system loop

+header ~forth_system, ~forth_system_n, forth, forth_n
	+forth
forth_system_c:
	+token lit
	+value banner_text
	+token count, type, cr
	+token decimal, false, state, poke, xsst
; Create the root Forth dictionary, has to be in writable memory
; set _current to zero - this will trigger a different path in CREATE
; connecting the new word to this word in LFA
	+token zero, blit
	+bvalue _current
	+token poke
	+token here, lit
	+value wlist
	+token count, xwordlist
; now set this new word as the first item in order (non-mutable) and current
	+token blit
	+bvalue _current
	+token poke, minusone, set_order
	+token context, poke	; finally, set the backlink to be the newly created word NFA
;
	+token lit
	+value autorun
	+token count, included, bbranch
	+baddress forth_system_1
forth_system_r:
	+token decimal, false, state, poke, xsst
forth_system_1:
	+token interpret, branch
	+address forth_system_1
banner_text:
	+string 14, "FORTH TX16 1.1"
autorun:
	+string 11, "AUTORUN.FTH"
wlist:
	+string 14, "FORTH-WORDLIST"

; ==============================================================================

end_of_image:

!symbollist "symbols.txt"
