; Forth system for Commander X16 - port of Forth Model T
; by Vasyl Tsvirkunov
; At this point the compliance status is:
; * Forth-2012 System
; * Providing the Core Extensions word set
; * Providing the Double-Number word set
; * Providing the Double-Number Extensions word set
; * Providing the File Access word set
; * Providing the File Access Extensions word set
; * Providing the Search-Order word set
; * Providing the Search-Order Extensions word set
; In addition, some words from String, Programming-Tools, and Facility sets are provided.
; File Access functionality is limited by the platform

; This system will pass all standard tests for Core, Core Extensions, Double-Number, and Search-Order.
; The supplied subset of Facility word set is sufficient to pass that test completely (only five words are tested).
; The partial Programming-Tools and String wordsets are compliant and will also pass individual tests.
; With dynamic-memory-allocation package by Ulrich Hoffmann installed the implementation will pass the
; Memory-Allocation tests (the implementation is slightly non-compliant and fails on negative sizes
; passed to ALLOCATE and RESIZE, those need to be patched for 100% clean test).
; Optional Block, Exception, and Locals sets are not implemented. No Floating-Point either.

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
; Added Search Order
; Switch to token threaded code and optimized many places

; Peculiarities:
;	C64 PETSCII charset does not have backslash. Pound symbol is used instead
;	It does not have tilde either. Not used in standard words, but it is used in test suite

; C64/Commander X16 prolog. "1 SYS 2061"
; Make sure the file is built in CBM mode (extra $01,$08 at the beginning)

* = $0801
start_of_image:
    !byte $0b,$08,$01,$00,$9e,$32,$30,$36,$31,$00,$00,$00

; KERNAL entries
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
; get two 1K stacks.
; However, to make implementation more efficient, the very top element of the data stack
; is separated on the zero page. The safe areas against overflow/underflow are 4 cells vs
; 8 cells in Forth Model T. Return stack does not use safe areas, as it is rarely an issue
; (the system is likely to crash and burn at that point already).

SSAFE = 4		; Number of reserved cells on the both sides of data stack for protection
RSIZE = $0400	; Return stack in bytes
DSIZE = $0400	; Data stack in bytes
TOKEN_COUNT = $0600	; Maximal number of available tokens in token threaded model, must be divisible by 256

WORDLISTS = 10	; Maximum number of allocated wordlists including FORTH-WORDLIST
MAXORDER = 10	; Maximum number of wordlists in search order

; In case if these get moved - important requirement, stacks must be aligned on 16-bit boundary;
; the token table must be aligned on page boundary for performance reasons

NAMEMASK = 31 ; vocabulary entries can have up to 32 characters
IMM_FLAG = 128 ; flag for immediate words
VAL_TRUE = -1 ; required by current standard
VAL_FALSE = 0
JSR_INSTR = $20 ; DOES> needs to emit JSR opcode
JMP_INSTR = $4C ; in direct threading each Forth word stars with JMP CALL
RTS_INSTR = $60	; in size-optimized direct threading this is the entire prolog of Forth calls

NEW_LINE = $0D

!macro zero_page_begin .start {
	!set __zpaddr = .start
}

!macro zero_page_end .check {
	!if __zpaddr > .check {
		!error "Out of zero page storage area!"
	}
}

!macro zpbyte ~.name {
	.name = __zpaddr
	!set __zpaddr = __zpaddr+1
}

!macro zpword ~.name {
	.name = __zpaddr
	!set __zpaddr = __zpaddr+2
}

!macro zpbytearray ~.name, .size {
	.name = __zpaddr
	!set __zpaddr = __zpaddr+.size
}

!macro zpwordarray ~.name, .size {
	.name = __zpaddr
	!set __zpaddr = __zpaddr+2*.size
}

+zero_page_begin $22	; Commander X16 user space $22-7F (94 bytes)

+zpword ~_ri					; inner interpreter registers
+zpword ~_w
+zpword ~_rstack				; stack pointers
+zpword ~_dstack
+zpword ~_dtop				; the very top element of the data stack
+zpword ~_rscratch			; four scratch registers used in multiple algorithms (sometimes aliased)
+zpword ~_wscratch
+zpword ~_scratch
+zpword ~_scratch_1
+zpword ~_scratch_2

+zpword ~_here
+zpword ~_base
+zpword ~_latest
+zpbyte ~_current
+zpword ~_state
+zpbyte ~_sflip				; flip-flop for S" buffer selection
+zpword ~_ibufcount			;number of used buffers
+zpword ~_source				; pointer to the current source
+zpword ~_openfiles			; bitfield for files currently open to translate from C64 to Forth opening semantics

+zpbyte ~_stopcheck

+zpword ~_hightoken

; Search-Order support
+zpwordarray ~_vocs, WORDLISTS
+zpwordarray ~_vocsref, WORDLISTS
+zpbyte ~_numvocs

+zpbytearray ~_context, MAXORDER
+zpbyte ~_numorder

+zero_page_end $7f

!macro high_memory_begin .addr {
	!set __hm_addr = .addr
}

!macro high_memory_end ~.label {
	.label = __hm_addr
}

!macro hmbuffer ~.name, .size {
	.name = __hm_addr - .size
	!set __hm_addr = .name
}

+high_memory_begin $9f00

+hmbuffer ~TOKENS, 2*TOKEN_COUNT	; token lookup table (XT->CFA)
+hmbuffer ~RSTACK, RSIZE		; return stack
+hmbuffer ~DSTACK, DSIZE		; data stack

+hmbuffer ~_ibuf, 7*100			; seven 100-char buffers for INCLUDE-FILE
+hmbuffer ~_sbuf, 2*100			; two 100-char buffers for S" / S\"
+hmbuffer ~_fnamebuf, 100		; buffer for filename storage only

+hmbuffer ~_tib, 100			; input buffer (reserving 100 bytes although only 81 are really needed)
+hmbuffer ~_wordbuf, 100		; buffer to hold result of WORD (reserving 100 bytes)
+hmbuffer ~_pad, 100			; PAD buffer
+hmbuffer ~_hld, 100			; pointer for pictured numeric output, the 98-byte buffer follow
_hldend = _hld + 100

+hmbuffer ~_sourcestack_bottom, 120		; end of the stack for sources (120 bytes to accomodate 7 files and default)
_sourcestack = _sourcestack_bottom + 120

+high_memory_end ~MEMTOP

RSTACK_INIT = RSTACK + RSIZE - 2
DSTACK_INIT = DSTACK + DSIZE - 2*SSAFE - 2
STACKLIMIT = DSIZE - 4*SSAFE


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
!macro header ~.token, ~.label_n, .name, .flags {
.label_n:
	!byte len(.name) +.flags
	!text .name
	!if .label_n-__prev_n < 128 {
		!byte .label_n-__prev_n
	} else {
		!if __prev_n = 0 {
			!byte 0
		} else {
			!byte ((.label_n-__prev_n) >> 8) | $80, (.label_n-__prev_n) & $FF
		}
	}
.token = __prev_token + 1
	!set __prev_token = .token
	!set __prev_n = .label_n
}

!macro header ~.token, ~.label_n, .name {
	+header ~.token, ~.label_n, .name, 0
}

!macro header ~.token, ~.label_n {
	+header ~.token, ~.label_n, "", 0
}

!macro header {
.label_n:
	!byte 0
	!byte 0
.token = 15	; tokens 0-15 have special meaning extending tokens past 8 bit
	!set __prev_token = .token
	!set __prev_n = .label_n
}

!macro check_token_range {
	!if __prev_token >= $100 {
		!error "Out of short tokens!"
	} else if __prev_token >= $fa {
		!warn "Running low on short tokens!"
	}
}

; Beginning of a compiled Forth word. Tokens follow
!macro forth {
	rts
}

; Beginning of a native code word. Assembler codes follow
!macro code {
}

!macro goforth {
	jsr contforth
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
!macro string .s {
	!byte len(.s)
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

!macro literal .val {
	!ifdef .val {
		!if .val < 256 {
			+token blit
			+bvalue .val
		} else {
			+token lit
			+value .val
		}
	} else {
		+token lit			; all forward refs (not first pass) should be large values
		+value .val
	}
}

; Unfortunately, cannot create a single macro to automatically choose the right path -
; it causes "Symbol already defined" in ACME
!macro branch .addr {
	+token branch
	+address .addr
}

!macro branch_fwd .addr {
	+token bbranch
	+baddress .addr
}

!macro qbranch .addr {
	+token qbranch
	+address .addr
}

!macro qbranch_fwd .addr {
	+token qbbranch
	+baddress .addr
}

; This should be placed right before a word to create an identifiable abort message
!macro error_message ~.label {
.label:
	+literal '?'
	+token emit, xabortq
}

; ==============================================================================
; Initialize the system

	lda #<forth_system_c
	ldx #>forth_system_c
	+stax _ri				; _w does not need to be initialized

	+init_rstack
	+init_dstack

	lda #0
	sta _sflip
	sta _ibufcount
	sta _ibufcount+1

	sta _tib
	sta _wordbuf

	sta _openfiles+1
	lda #7			; do not try to open 0-2
	sta _openfiles

	lda #<forth_system_n
	ldx #>forth_system_n
	+stax _latest
	
; This needs to be changed for split memory model, and it is the only change
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
; Structure of a vocabulary word in token threaded code:
; offset     length      meaning
;    0          1        n - length of the name and flags (NFA)
;    1          n        name
;    n+1     1 or 2      link to the previous word (LFA)
;  [n+2(3)]     1        prologue, RTS in current model (CFA)
;    ...        x        tokens (PFA)
; This may be quite confusing. LFA is actually a variable length value - it is an
; _offset_ to the previous word (counting between NFAs). If is less than 128, it will
; take one byte. Otherwise it is MSB first with high bit set in MSB - so if the first
; byte has the high bit set, there will be two bytes. One exception - if the first
; byte is $ff, it is the only byte and it links to the last word in core (split memory
; support).
; Native word does not have a prologue, can start immediately after the LFA and should
; be ended with "jmp next". Forth words start with prologue consisting of the instruction
; RTS, that would cause an immediate jump to CALL.
; Most elements of a word definition are hidden behind macros. This is both for readability
; and to make it easier to modify in the future.
; All core words only refer to tokens below 256, so all token references take one byte.
; Note that the tokens are encoded with MSB first, but that MSB is always very small, and
; the implementation treats two-byte tokens as essentially two tokens (see trick in NEXT).

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

; Call to this subroutine (JSR contforth) allows to switch execution from native to Forth. One particular use case
; is error handling as it is easier to setup ABORT" from Forth
contforth:
	pla		; Note that 6502 has the address on the stack off by one, but the code in CALL will compensate by one
	sta _w
	pla
	sta _w+1
	rts

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

; Binary search for 16-bit value - adapted from code found online
bslow = _rscratch
bshigh = _wscratch
bsmid = _scratch
bsvalue = _dtop

binsearch:
bsloop:
	sec
	lda bshigh
	sbc bslow
	tax
	lda bshigh+1
	sbc bslow+1
	bcc bsdone		; LOW is ABOVE high, no more locations to check. C is 0
	lsr
	tay
	txa
	ror
	and #$fe		; Align to even
	adc bslow
	sta bsmid
	tya
	adc bslow+1
	sta bsmid+1		; At this point MID = LOW + (HIGH-LOW)/2 aligned
	lda bsvalue+1	; Check MSB and perform one of three possible steps below
	ldy #1
	cmp (bsmid),y
	beq bschklsb
	bcc bsmodhigh
bsmodlow:			; Move LOW to be one word above MID
	lda bsmid
	adc #1			; C is 1 as it can only be reached by BCS or fall through BCC
	sta bslow
	lda bsmid+1
	adc #0
	sta bslow+1
	jmp bsloop
bschklsb:			; MSB is matching. Check LSB and adjust HIGH or LOW depending on the value
	lda bsvalue
	dey
	cmp (bsmid),y
	beq bsdone		; MID is pointing at the exact value (and C is 1)
	bcs bsmodlow
bsmodhigh:			; Move HIGH to be one word below MID
	lda bsmid
	sbc #1			; C is 0 at this point as it is only reachable by BCC or fall through BCS
	sta bshigh
	lda bsmid+1
	sbc #0
	sta bshigh+1
	jmp bsloop
bsdone:
	rts


generate_token_table:
	; start scanning from the last word NFA
	lda #<forth_system_n
	ldx #>forth_system_n
	+stax _rscratch
	
	; set current token to be the last
	lda #(forth_system<<1)&$ff
	sta _scratch
	clc
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
; Forth vocabulary starts here. This special header form reserves 16 tokens for
; extended token range. This essentially gives 4K-16 possible tokens maximum,
; which should be enough for any configuration (that many tokens will consume
; an 8K table, quite wasteful for common case)
+header

; ==============================================================================
; Hidden words to go from one vocabulary word part to another. All of these take
; one address from the top of the stack and return another

+header ~nfatolfa, ~nfatolfa_n
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

+header ~lfatocfa, ~lfatocfa_n
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

+header ~cfatolfa, ~cfatolfa_n
	+forth
	+token dup, twominus, cpeek
	+literal $80
	+token and_op
	+qbranch_fwd cfatolfa_1
	+token oneminus
cfatolfa_1:
	+token oneminus, exit


; This is one of the less trivial transitions in the current model. There is a trap here -
; using namemask to isolate just the low 5 bits of the length field is unsafe, it may
; misidentify bytes as length bytes (e.g., "AB" will break it). This means that there is only
; one bit for flags in that field.
+header ~lfatonfa, ~lfatonfa_n	; known in some dialects as L>NAME
	+forth
	+token oneminus, zero
lfatonfa_next:
	+token over, cpeek
	+literal $7f
	+token and_op, over, notequal
	+qbranch_fwd lfatonfa_found
	+token swap, oneminus, swap, oneplus, dup
	+literal 31
	+token greater
	+qbranch lfatonfa_next
	+token drop, drop, zero, exit
lfatonfa_found:
	+token drop, exit

+header ~xttocfa, ~xttocfa_n
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
	+stax _dtop
	jmp next


; ==============================================================================
; A very important word to translate CFA to XT
; TODO: this is perf critical for interpretation. Optimize to use binary search
; (token table has two sorted sections)

+header ~cfatoxt, ~cfatoxt_n
	+code

; The code needs to be prepared for split system with core and compiled words in
; different parts of RAM. For that purpose binsearch should be called twice (words
; are sorted within each part)
; TOKENS to TOKENS+2*forth_system

; Prep for binsearch: _rscratch points to TOKENS, _wscratch is TOKENS+2*_hightoken
	lda #0
	sta _rscratch
	lda #<forth_system
	asl
	sta _wscratch
	sta _scratch_1
	lda #>TOKENS
	sta _rscratch+1
	lda #>forth_system
	rol
	adc #>TOKENS
	sta _wscratch+1
	sta _scratch_1+1

	jsr binsearch
	bcs cfatoxt_found

; TOKENS+2*forth_system+2 to TOKENS+2*_hightoken
	lda _scratch_1	; No need to clear C here as it was left 0 by the previous binsearch!
	adc #2
	sta _rscratch
	lda _scratch_1+1
	adc #0
	sta _rscratch+1
	lda _hightoken
	asl
	sta _wscratch
	lda _hightoken+1
	rol
	adc #>TOKENS
	sta _wscratch+1

	jsr binsearch

cfatoxt_found:		; Unless the system is broken, something has to be found and C is set!
	lda _scratch
	sbc #<TOKENS
	sta _dtop
	lda _scratch+1
	sbc #>TOKENS
	lsr
	sta _dtop+1
	ror _dtop
	jmp next

; ==============================================================================
; code exit
; code execute
; : quit (sst) ;code
; : abort begin depth 0> while drop again begin depth <0 while 0 again quit ;
;

; EXIT is used to return from any word.
+header ~exit, ~exit_n, "EXIT"
	+code return

; Execute the word by address on the stack
+header ~execute, ~execute_n, "EXECUTE"
	+code invoke

; TODO: should also call (SST) in QUIT
; Reset data stack and perform QUIT.
+header ~abort, ~abort_n, "ABORT"
	+code
abort_c:
	+init_dstack
	jmp quit_c

;
; : (sst) _sourcestack
;         2- 0 over ! 2- _tib over ! 2- 0 over ! 2- 0 over ! 2- 4 over !
;         _source ! 0 dup _sflip ! _ibufcount ! ; nonstandard
;

+header ~xsst, ~xsst_n			; Reset source stack
	+forth
	+literal _sourcestack
	+token twominus, zero, over, poke			; #TIB
	+token twominus
	+literal _tib
	+token over, poke							; TIB
	+token twominus, zero, over, poke			; >IN
	+token twominus, zero, over, poke			; SOURCE-ID
	+token twominus
	+literal $04
	+token over, poke		; standard input has 4 parameters: 0, >IN, TIB, #TIB
	+literal _source
	+token poke
	+token zero, dup
	+literal _sflip
	+token cpoke
	+literal _ibufcount
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

+header ~zero, ~zero_n, "0"
	+code doconst
	+value $0000

+header ~one, ~one_n, "1"
	+code doconst
	+value $0001

+header ~two, ~two_n, "2"
	+code doconst
	+value $0002

+header ~minusone, ~minusone_n, "-1"
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

+header ~add, ~add_n, "+"
	+code
	+dpop
	clc
	adc _dtop
	sta _dtop
	txa
	adc _dtop+1
	sta _dtop+1
	jmp next

+header ~sub, ~sub_n, "-"
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

+header ~mult, ~mult_n, "*"
	+forth
	+token mmult, drop, exit

+header ~divmod, ~divmod_n, "/MOD"
	+forth
	+token tor, stod, rfrom, smrem, exit

+header ~multdivmod, ~multdivmod_n, "*/MOD"
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

+header ~abs, ~abs_n, "ABS"
	+code
	lda _dtop+1
	bmi negate_c
	jmp next

+header ~negate, ~negate_n, "NEGATE"
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

+header ~oneplus, ~oneplus_n, "1+"
	+code
	inc _dtop
	bne +
	inc _dtop+1
+:
	jmp next

+header ~oneminus, ~oneminus_n, "1-"
	+code
	lda _dtop
	bne +
	dec _dtop+1
+:
	dec _dtop
	jmp next

+header ~twoplus, ~twoplus_n, "2+"
	+code
	+ldax _dtop
	+incax 2
	+stax _dtop
	jmp next

+header ~twominus, ~twominus_n, "2-"
	+code
	+ldax _dtop
	+decax 2
	+stax _dtop
	jmp next

+header ~twodiv, ~twodiv_n, "2/"
	+code
	lda _dtop+1
	cmp #$80		; 6502 does not have native arithmetic shift right 
	ror _dtop+1
	ror _dtop
	jmp next

+header ~twomult, ~twomult_n, "2*"
	+code
	asl _dtop
	rol _dtop+1
	jmp next

;
; code lshift
; code rshift
;

+header ~lshift, ~lshift_n, "LSHIFT"
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
			
+header ~rshift, ~rshift_n, "RSHIFT"
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

+header ~stod, ~stod_n, "S>D"
	+forth
	+token dup, zerolt
	+qbranch_fwd stod_1
	+token minusone, exit
stod_1:
	+token zero, exit

; Optional Double=number word set
+header ~dnegate, ~dnegate_n, "DNEGATE"
	+forth
	+token invert, swap, invert, swap, one, mplus, exit 

; Optional Double-numbler word set
+header ~dabs, ~dabs_n, "DABS"
	+forth
	+token dup, zerolt
	+qbranch_fwd dabs_1
	+token dnegate
dabs_1:
	+token exit

;
; : sm/rem 2dup xor >r ( Sign of the quotient) over >r ( Sign of the remainder)
;          abs >r dabs r> um/mod
;          swap r> 0< if negate then
;          swap r> 0< if negate then ;
;

+header ~smrem, ~smrem_n, "SM/REM"
	+forth
	+token twodup, xor, tor, over, tor
	+token abs, tor, dabs, rfrom, ummod
	+token swap, rfrom, zerolt
	+qbranch_fwd smrem_1
	+token negate
smrem_1:
	+token swap, rfrom, zerolt
	+qbranch_fwd smrem_2
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

+header ~ummod, ~ummod_n, "UM/MOD"
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

+header ~udmod, ~udmod_n, "UD/MOD"
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

+header ~ummult, ~ummult_n, "UM*"
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

+header ~mmult, ~mmult_n, "M*"
	+forth
	+token twodup, xor, tor, abs, swap
	+token abs, ummult, rfrom, zerolt
	+qbranch_fwd mmult_1
	+token dnegate
mmult_1:
	+token exit

+header ~udmult, ~udmult_n, "UD*"
	+forth
	+token dup, tor, ummult, drop, swap
	+token rfrom, ummult, rot, add, exit

;
; : m+ s>d d+ ;
;

; From the optional Double-number word set
+header ~mplus, ~mplus_n, "M+"
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

+header ~and_op, ~and_n, "AND"
	+code
	+dpop
	and _dtop
	sta _dtop
	txa
	and _dtop+1
	sta _dtop+1
	jmp next

+header ~or, ~or_n, "OR"
	+code
	+dpop
	ora _dtop
	sta _dtop
	txa
	ora _dtop+1
	sta _dtop+1
	jmp next

+header ~xor, ~xor_n, "XOR"
	+code
	+dpop
	eor _dtop
	sta _dtop
	txa
	eor _dtop+1
	sta _dtop+1
	jmp next

; Note that NOT has been removed from the standard.
+header ~invert, ~invert_n, "INVERT"
	+forth
	+token minusone, xor, exit

; Find lowest zero (free) bit index
+header ~freebit, ~freebit_n
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

+header ~setbit, ~setbit_n
	+forth
	+token dup, peek, rot, one, swap
	+token lshift, or, swap, poke, exit

+header ~clearbit, ~clearbit_n
	+forth
	+token dup, peek, rot, one, swap, lshift
	+token invert, and_op, swap, poke, exit

; ==============================================================================
; Comparisons

;
; code 0=
; code 0<
;

+header ~zeroeq, ~zeroeq_n, "0="
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

+header ~zerolt, ~zerolt_n, "0<"
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

+header ~zerogt, ~zerogt_n, "0>"
	+forth
	+token zero, swap, less, exit
			
+header ~zerone, ~zerone_n, "0<>"
	+forth
	+token zeroeq, zeroeq, exit

+header ~equal, ~equal_n, "="
	+forth
	+token sub, zeroeq, exit

+header ~notequal, ~notequal_n, "<>"
	+forth
	+token sub, zerone, exit


;
; code <
;
; Careful here. Some implementations have it as ": < - 0< ;" and it works... sometimes.
; Signed comparison on 6502 is surprisingly non-trivial. Refer to http://www.6502.org/tutorials/compare_beyond.html
; for details and tutorial
+header ~less, ~less_n, "<"
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

+header ~greater, ~greater_n, ">"
	+forth
	+token swap, less, exit


;
;	code u<
;
+header ~uless, ~uless_n, "U<"
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

+header ~ugreater, ~ugreater_n, "U>"
	+forth
	+token swap, uless, exit

;
;	-1 constant true
;	0 constant false
;

+header ~true, ~true_n, "TRUE"
	+code doconst
	+value VAL_TRUE

+header ~false, ~false_n, "FALSE"
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

+header ~dup, ~dup_n, "DUP"
	+code
	+ldax _dtop
	+dpush
	jmp next

+header ~drop, ~drop_n, "DROP"
	+code
	+dpop
	jmp next

+header ~over, ~over_n, "OVER"
	+code
	ldy #3
	lda (_dstack),y
	tax
	dey
	lda (_dstack),y
	+dpush
	jmp next

+header ~swap, ~swap_n, "SWAP"
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

+header ~nip, ~nip_n, "NIP"
	+forth
	+token swap, drop, exit

+header ~tuck, ~tuck_n, "TUCK"
	+forth
	+token swap, over, exit

;
; : rot >r swap r> swap ;
;

+header ~rot, ~rot_n, "ROT"
	+forth
	+token tor, swap, rfrom, swap, exit

;
;	code pick
;	code roll ; using reference implementation from forth-standard.org instead
;
+header ~pick, ~pick_n, "PICK"
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

+header ~roll, ~roll_n, "ROLL"
	+forth
	+token qdup
	+qbranch_fwd roll_1
	+token swap, tor, oneminus, roll, rfrom, swap
roll_1:
	+token exit

;
;	code depth
;

+header ~depth, ~depth_n, "DEPTH"
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
;	: 2swap rot >r rot r> ;
;	: 2over >r >r 2dup r> r> 2swap ;
;

+header ~twodrop, ~twodrop_n, "2DROP"
	+forth
	+token drop, drop, exit

+header ~twodup, ~twodup_n, "2DUP"
	+forth
	+token over, over, exit

+header ~twoswap, ~twoswap_n, "2SWAP"
	+forth
	+token rot, tor, rot, rfrom, exit

+header ~twoover, ~twoover_n, "2OVER"
	+forth
	+token tor, tor, twodup, rfrom, rfrom, twoswap, exit

;
;	: ?dup dup if dup then ;
;

+header ~qdup, ~qdup_n, "?DUP"
	+forth
	+token dup
	+qbranch_fwd qdup_1
	+token dup 
qdup_1:
	+token exit

; ==============================================================================
; Internal helpers for DO/LOOP words - these are essentially compiled in.
; (DO) stores on the return stack: leaveaddr, limit, current, (ret) 
+header ~xdo, ~xdo_n	; (DO)
	+forth
	+token rfrom, dup, peek, tor	; forward ref for LEAVE
	+token rot, tor, swap, tor
	+token twoplus, tor				; step over the actual forward ref
	+token exit

+header ~xqdo, ~xqdo_n	; (?DO)
	+forth
	+token twodup, equal
	+qbranch_fwd xqdo_1
	+token twodrop, rfrom, peek, tor, exit
xqdo_1:
	+token rfrom, dup, peek, tor	; forward ref for LEAVE
	+token rot, tor, swap, tor
	+token twoplus, tor				; step over the actual forward ref
	+token exit
			
; and (LOOP) adjusts the values on rstack or just drops the top three values from it to exit
+header ~xloop, ~xloop_n	; (LOOP)
	+forth
	+token rfrom				; return address is only needed to get the backref
	+token rfrom, oneplus			; new value of current
	+token rat, over, equal
	+qbranch_fwd xloop_1
	+token drop, drop, rdrop, exit	; exit the loop (leaveaddr on the rstack)
xloop_1:
	+token tor, peek, tor, exit		; continue the loop

+header ~xploop, ~xploop_n	; (+LOOP)
	+forth
	+token rfrom, swap		; return address is only needed to get the backref / addr, step
	+token dup, rat, add			; preserve step value and get new value of current / addr, step, newcur
	+token rfrom, rat, sub			; diff limit and new current / addr, step, newcur, olddiff
	+token rot, over, xor, zerolt, swap ; new diff and step have different signs? / addr, newcur, step^olddiff<0, olddiff
	+token two, pick, rat, sub		; diff limit and previous current / addr, newcur, s^d, olddiff, newdiff
	+token xor, zerolt, and_op
	+qbranch_fwd xploop_1  ; or diffs before and after have different signs / newdiff^olddiff < 0
	+token drop, drop, rdrop, exit	; exit the loop (leaveaddr on the rstack)
xploop_1:
	+token tor, peek, tor, exit		; continue the loop

; The following three may be a bit high-level, but they make writing other words easier
+header ~i, ~i_n, "I"
	+forth
	+token rfrom, rat, swap, tor, exit

+header ~j, ~j_n, "J"
	+forth
	+token rfrom, rfrom, rfrom, rfrom, rfrom, dup, tor
	+token swap, tor, swap, tor, swap, tor, swap, tor
	+token exit

+header ~leave, ~leave_n, "LEAVE"
	+forth
	+token rdrop, rdrop, rdrop, exit

; ==============================================================================
; Standard cell/char size words and alignment (which do nothing on this architecture)

;
; : cell+ 2+ ;
; : cells 2* ;
; : char+ 1+ ;
; : chars ;
; : align ;
; : aligned ;
;

+header ~cellplus, ~cellplus_n, "CELL+"
	+forth
	+token twoplus, exit
			
+header ~cells, ~cells_n, "CELLS"
	+forth
	+token twomult, exit
			
+header ~charplus, ~charplus_n, "CHAR+"
	+forth
	+token oneplus, exit
			
+header ~chars, ~chars_n, "CHARS"
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

+header ~rfrom, ~rfrom_n, "R>"
	+code
	+rpop
	+dpush
	jmp next

+header ~tor, ~tor_n, ">R"
	+code
	+dpop
	+rpush
	jmp next

+header ~rat, ~rat_n, "R@"
	+code
	ldy #3
	lda (_rstack),y
	tax
	dey
	lda (_rstack),y
	+dpush
	jmp next

+header ~rdrop, ~rdrop_n, "RDROP"
	+code
	+rpop
	jmp next

+header ~twotor, ~twotor_n, "2>R"
	+code
	+dpop
	+stax _rscratch
	+dpop
	+rpush
	+ldax _rscratch
	+rpush
	jmp next

+header ~tworfrom, ~tworfrom_n, "2R>"
	+code
	+rpop
	+stax _rscratch
	+rpop
	+dpush
	+ldax _rscratch
	+dpush
	jmp next

+header ~tworat, ~tworat_n, "2R@"
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

+header ~peek, ~peek_n, "@"
	+code
	ldy #1
	lda (_dtop),y
	tax
	dey
	lda (_dtop),y
	+stax _dtop
	jmp next

+header ~cpeek, ~cpeek_n, "C@"
	+code
	ldy #0
	lda (_dtop),y
	sta _dtop
	sty _dtop+1
	jmp next

+header ~poke, ~poke_n, "!"
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

+header ~cpoke, ~cpoke_n, "C!"
	+code
	+dpop
	+stax _wscratch
	+dpop
	ldy #0
	sta (_wscratch),y
	jmp next

+header ~twopoke, ~twopoke_n, "2!"
	+forth
	+token tuck, poke, cellplus, poke, exit

+header ~twopeek, ~twopeek_n, "2@"
	+forth
	+token dup, cellplus, peek, swap, peek, exit

; ==============================================================================
; Literal support
;
; code lit
; Alternative but slower: : lit r@ peek r> cell+ >r ; nonstandard
;

; This is being compiled by LITERAL - will take the next 16-bit value and put it
; on stack
+header ~lit, ~lit_n	; LIT
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

; This is a shortcut for core use only - will take the 8-bit value instead. LITERAL
; does not compile it yet, but it can in theory.
+header ~blit, ~blit_n
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

+header ~base, ~base_n, "BASE"
	+code doconst
	+value _base

+header ~bhash, ~bhash_n, "<#"
	+forth
	+literal _hldend
	+literal _hld
	+token poke, exit

+header ~hash, ~hash_n, "#"
	+forth
	+token base, peek, udmod, rot
	+literal '0'
	+token add
	+token dup
	+literal '9'
	+token greater
	+qbranch_fwd hash_1
	+literal 7
	+token add
hash_1:
	+token hold, exit

+header ~hashs, ~hashs_n, "#S"
	+forth
hashs_1:
	+token hash, twodup, or, zeroeq
	+qbranch hashs_1
	+token exit

+header ~hashb, ~hashb_n, "#>"
	+forth
	+token twodrop
	+literal _hld
	+token peek
	+literal _hldend
	+token over, sub, exit

+header ~hold, ~hold_n, "HOLD"
	+forth
	+literal _hld
	+token peek, one, sub, dup
	+literal _hld
	+token poke, cpoke, exit

+header ~sign, ~sign_n, "SIGN"
	+forth
	+token zerolt
	+qbranch_fwd sign_1
	+literal '-'
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

+header ~ddotr, ~ddotr_n, "D.R"
	+forth
	+token tor, dup, tor, dabs, bhash, hashs, rfrom
	+token sign, hashb, rfrom, over, sub, spaces, type, exit

+header ~ddot, ~ddot_n, "D."
	+forth
	+token zero, ddotr, space, exit

+header ~dot, ~dot_n, "."
	+forth
	+token stod, ddot, exit

;
; : decimal 10 base ! ;
; : hex 16 base ! ;
;

+header ~decimal, ~decimal_n, "DECIMAL"
	+forth
	+literal 10
	+token base, poke, exit

+header ~hex, ~hex_n, "HEX"
	+forth
	+literal 16
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

+header ~incpoke, ~incpoke_n, "+!"
	+forth
	+token dup, peek, rot, add, swap, poke, exit

+header ~here, ~here_n, "HERE"
	+forth
	+literal _here
	+token peek, exit


+header ~allot, ~allot_n, "ALLOT"
	+forth
	+token dup, unused, greater
	+qbranch_fwd allot_ok
	+token xabortq
	+string "?MEM"
allot_ok:
	+literal _here
	+token incpoke, exit

+header ~unused, ~unused_n, "UNUSED"
	+forth
	+literal MEMTOP
	+token here, sub, exit

+header ~comma, ~comma_n, ","
	+forth
	+token here, two, allot, poke, exit

+header ~ccomma, ~ccomma_n, "C,"
	+forth
	+token here, one, allot, cpoke, exit

; ==============================================================================
; Support for branching. These are compiled by most control words
; There is an exotic way to implement BRANCH as
; : BRANCH R> @ >R ;
; This approach does not work for ?BRANCH due to chicken-and-egg problem

;
; code branch nonstandard
; code ?branch nonstandard
;

; Unconditional jump to the address following the token
+header ~branch, ~branch_n	; BRANCH
	+code
branch_c:
	ldy #1
	lda (_ri),y
	tax
	dey
	lda (_ri),y
	+stax _ri
	jmp next

; Jump to the address if top of the stack is FALSE. Skip the address and
; continue otherwise
+header ~qbranch, ~qbranch_n	; ?BRANCH aka 0BRANCH
	+code
	+dpop
	stx _rscratch
	ora _rscratch
	beq branch_c
	+ldax _ri
	+incax 2
	+stax _ri
	jmp next

; Shorthand variety of BRANCH for core use - threat the next byte as a
; relative _forward only_ offset instead of address
+header ~bbranch, ~bbranch_n
	+code
bbranch_c:
	ldy #0
	lda (_ri),y
	clc
	adc _ri
	sta _ri
	bcc +
	inc _ri+1
+
	jmp next

; Similarly, shorthand for ?BRANCH
+header ~qbbranch, ~qbbranch_n
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

+header ~tib, ~tib_n, "TIB"
	+forth
	+literal _source
	+token peek, twoplus, twoplus, twoplus, peek, exit

+header ~ptrin, ~ptrin_n, ">IN"
	+forth
	+literal _source
	+token peek, twoplus, twoplus, exit

+header ~numtib, ~numtib_n, "#TIB"
	+forth
	+literal _source
	+token peek, twoplus, twoplus, twoplus, twoplus, exit

+header ~source, ~source_n, "SOURCE"
	+forth
	+token tib, numtib, peek, exit

+header ~sourceid, ~sourceid_n, "SOURCE-ID"
	+forth
	+literal _source
	+token peek, twoplus, peek, exit

+header ~accept, ~accept_n, "ACCEPT"
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
+header ~refill, ~refill_n, "REFILL"
	+forth
	+token sourceid, zerolt
	+qbranch_fwd refill_1
	+token false, exit	; "EVALUATE" - no refill
refill_1:
	+token sourceid, zeroeq
	+qbranch_fwd refill_2
	+token cr
	+literal prompt
	+token count, type, cr, tib
	+literal 80
	+token accept, numtib, poke, zero, ptrin, poke, true, exit	; console
refill_2:
	+token sourceid, fileposition, drop
	+literal _source
	+token peek
	+literal 10
	+token add, twopoke
	+token tib
	+literal 98
	+token sourceid, readline, zeroeq, and_op
	+qbranch_fwd refill_3
	+token numtib, poke, zero, ptrin, poke, true, exit	 ; file (note that the position is saved _before_ the refill)
refill_3:
	+token drop, false, exit
prompt:
	+string "OK"

; ==============================================================================
; Some basic text output

; code emit

+header ~emit, ~emit_n, "EMIT"
	+code
	+dpop
	jsr CHROUT
	jmp next

; : cr 13 emit ;

+header ~cr, ~cr_n, "CR"
	+forth
	+literal NEW_LINE
	+token emit, exit

; ' ' constant bl

+header ~bl, ~bl_n, "BL"
	+code doconst
	+value ' '

; : space bl emit ;

+header ~space, ~space_n, "SPACE"
	+forth
	+token bl, emit, exit

; This one is a little bit weird, abusing the semantics of COUNT
; : type 0 ?do count emit loop drop ;
; 
+header ~type, ~type_n, "TYPE"
	+forth
	+token zero, xqdo
	+address type_out
type_loop:
	+token count, emit, xloop
	+address type_loop
type_out:
	+token drop, exit

; : count dup 1+ swap c@ ;

+header ~count, ~count_n, "COUNT"
	+forth
	+token dup, oneplus, swap, cpeek, exit

; ==============================================================================
; Word lookup. This is where the complex things begin.

+header ~word, ~word_n, "WORD"
	+forth
	+token tor, source, swap, ptrin, peek, add
word_1:
	+token over, ptrin, peek, greater
	+qbranch_fwd word_2
	+token dup, cpeek
	+literal 127 ; a workaround for peculiar C64 annoyance
	+token and_op, rat, equal
	+qbranch_fwd word_2
	+token ptrin, peek, oneplus, ptrin, poke, oneplus
	+branch word_1
word_2:
	+token twodrop, rfrom, parse, dup
	+literal _wordbuf
	+token cpoke
	+literal _wordbuf
	+token oneplus, swap, cmove
	+literal _wordbuf
	+token exit

+header ~parse, ~parse_n, "PARSE"
	+forth
	+token tor, source, ptrin, peek, sub
	+token oneplus, tor, ptrin, peek, add, dup, zero
parse_1:
	+token over, cpeek, rfrom, oneminus, qdup
	+qbranch_fwd parse_3
	+token swap
	+literal 127
	+token and_op, rat, equal
	+qbranch_fwd parse_2 ; SAME AS ABOVE
	+token drop, nip, rdrop, ptrin
	+token dup, peek, oneplus, swap, poke, exit
parse_2:
	+token tor, swap, oneplus, swap, oneplus, ptrin
	+token dup, peek, oneplus, swap, poke
	+branch parse_1
parse_3:
	+token drop, nip, rdrop, exit

+header ~parsename, ~parsename_n, "PARSE-NAME"
	+forth
	+token source, swap, ptrin, peek, add
parsename_1:
	+token over, ptrin, peek, greater
	+qbranch_fwd parsename_2
	+token dup, cpeek, bl, equal
	+qbranch_fwd parsename_2
	+token ptrin, peek, oneplus, ptrin, poke, oneplus
	+branch parsename_1
parsename_2:
	+token twodrop, bl, parse, exit


+header ~tobody, ~tobody_n, ">BODY"
	+forth
	+token xttocfa, twoplus, oneplus, exit	; offset for JMP CREATED

; ==============================================================================

+header ~context, ~context_n		; known in some dialectes as CONTEXT, obsolete with Search-Order
	+forth
	+literal _current
	+token cpeek, cells
	+literal _vocs
	+token add, exit

+header ~latest, ~latest_n		; known in some dialectes as LATEST, non-standard
	+forth
	+literal _latest
	+token peek, exit

; Search-order words
; GET-ORDER and SET-ORDER use the reference implementation with the exception that
; both #order and context are byte size

; : GET-ORDER ( -- wid1 ... widn n )
;   #order @ 0 ?DO
;     #order @ I - 1- CELLS context + @
;   LOOP
;   #order @
; ; 

+header ~get_order, ~get_order_n, "GET-ORDER"
	+forth
	+literal _numorder
	+token cpeek, zero, xqdo
	+address getorder_done
getorder_loop:
	+literal _numorder
	+token cpeek, i, sub, oneminus
	+literal _context
	+token add, cpeek, xloop
	+address getorder_loop
getorder_done:
	+literal _numorder
	+token cpeek, exit

; : SET-ORDER ( wid1 ... widn n -0 )
;   DUP -1 = IF
;     DROP <push system default word lists and n>
;   THEN
;   DUP #order !
;   0 ?DO I CELLS context + ! LOOP
; ;
 
+error_message ~setorder_error
+header ~set_order, ~set_order_n, "SET-ORDER"
	+forth
	+token dup
	+literal MAXORDER+1
	+token less
	+qbranch setorder_error
	+token dup, minusone, equal
	+qbranch_fwd setorder_cont
	+token drop, zero, one		; FORTH-WORDLIST wid is 0
setorder_cont:
	+token dup
	+literal _numorder
	+token cpoke, zero, xqdo
	+address setorder_exit
setorder_loop:
	+token i
	+literal _context
	+token add, cpoke, xloop
	+address setorder_loop
setorder_exit:
	+token exit

; Structure of wordlist word
; - header
; - JMP doconst (constant semantics)
; - xt of self, used as wid
; - NULL as there are no items in the list yet

+header ~xwordlist, ~xwordlist_n
	+forth
	+literal _numvocs
	+token cpeek
	+literal WORDLISTS
	+token less
	+qbranch_fwd xwordlist_error
	+token xcreate
	+literal JMP_INSTR
	+token ccomma
	+literal doconst
	+token comma
	+literal _numvocs
	+token cpeek
	+token dup, comma
	+token dup, cells
	+token dup
	+literal _vocs
	+token add, zero, swap, poke
	+literal _vocsref
	+token add, latest, swap, poke
	+token dup, oneplus
	+literal _numvocs
	+token cpoke, exit
xwordlist_error:
	+token xabortq
	+string "?WLIST"

+header ~search_wordlist, ~search_wordlist_n, "SEARCH-WORDLIST"
	+forth
	+token cells
	+literal _vocs
	+token add, peek
	+token xfind, dup
	+qbranch_fwd sw_notfound
	+token dup, minusone, swap, cpeek
	+literal $80
	+token and_op
	+qbranch_fwd sw_notimm
	+token negate
sw_notimm:
	+token swap, nfatolfa, lfatocfa, cfatoxt
	+token swap
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

+header ~xfind, ~xfind_n		; (FIND) ( caddr, start_NFA -> NFA | 0 )
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
	eor (_dtop),y		; poor man's case insensitive compare ;)
	and #$5f
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
	cmp #$ff			; special case for link from compiled section to core (differential LFA may not be capable to do this one jump otherwise)
	beq xfind_linkcore
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

xfind_linkcore:
	lda #<forth_system_n
	sta _rscratch
	lda #>forth_system_n
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

; Close to the reference implementation:
; : find 0 #order @ 0 ?do
;        over count i cells context + @ search-wordlist
;        ?dup if
;             2swap 2drop leave
;             then
;        loop ;

+header ~find, ~find_n, "FIND"
	+forth
	+token zero
	+literal _numorder
	+token cpeek, zero, xqdo			; note c@ instead of @ - we have much less than 256 items
	+address find_exit
find_loop:
	+token over, count, i
	+literal _context
	+token add, cpeek
	+token search_wordlist
	+token qdup
	+qbranch_fwd find_next
	+token twoswap, twodrop, leave
find_next:
	+token xloop
	+address find_loop
find_exit:
	+token exit


+header ~xdigit, ~xdigit_n	; (DIGIT)
	+code
	lda _dtop
	cmp #$40
	bcc +
	and #$5f
+:
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

+header ~tonumber, ~tonumber_n, ">NUMBER"
	+forth
tonumber_1:
	+token dup, zerogt
	+qbranch_fwd tonumber_3									; no more digits left?
	+token over, cpeek, xdigit, dup, zerolt, zeroeq
	+qbranch_fwd tonumber_2	; not a possible digit?
	+token dup, base, peek, less
	+qbranch_fwd tonumber_2						; not a digit in current base?
	+token swap, oneminus, tor, swap, oneplus, tor, tor
	+token base, peek, udmult, rfrom, mplus, rfrom, rfrom
	+branch tonumber_1												; and repeat
tonumber_2:
	+token drop
tonumber_3:
	+token exit

+header ~number, ~number_n	; NUMBER
	+forth
	+token count, base, peek, tor
	+token dup
	+literal 3
	+token equal, two, pick, cpeek
	+literal 39
	+token equal, and_op			; character as 'c'
	+token two, pick, two, add, cpeek
	+literal 39
	+token equal, and_op
	+qbranch_fwd number_8
	+token drop, oneplus, cpeek
	+branch_fwd number_5
number_8:
	+token dup, one, greater
	+qbranch_fwd number_9
	+token over, cpeek
	+literal 35
	+token equal
	+qbranch_fwd number_11
	+token decimal
	+branch_fwd number_10
number_11:
	+token over, cpeek
	+literal 36
	+token equal
	+qbranch_fwd number_12
	+token hex
	+branch_fwd number_10
number_12:
	+token over, cpeek
	+literal 37
	+token equal
	+qbranch_fwd number_9
	+token two, base, poke
number_10:
	+token swap, oneplus, swap, oneminus
number_9:
	+token twodup, false, tor, over, cpeek
	+literal 45
	+token equal
	+qbranch_fwd number_1
	+token rdrop, true, tor, oneminus, swap, oneplus, swap
number_1:
	+token zero, dup, twoswap, tonumber, qdup
	+qbranch_fwd number_4
	+token one, equal, swap, cpeek
	+literal 46
	+token equal, and_op
	+qbranch_fwd number_7	; one unconverted char and it's '.'?
	+token rfrom
	+qbranch_fwd number_2
	+token dnegate
number_2:
	+token twoswap, twodrop, state, peek
	+qbranch_fwd number_3
	+token compile, lit, swap
	+token comma, compile, lit, comma
number_3:
	+branch_fwd number_6
number_4:
	+token drop, twoswap, twodrop, drop, rfrom
	+qbranch_fwd number_5
	+token negate
number_5:
	+token state, peek
	+qbranch_fwd number_6
	+token compile, lit, comma
number_6:
	+token rfrom, base, poke, exit
number_7:
	+token twodrop, type, xabortq
	+string " ?"


; ==============================================================================

+header ~nextword, ~nextword_n
	+forth
	+token dup, nfatolfa
; extract the offset from the LFA (this code may look verbose but the
; native 6502 code is actually about the same in size)
	+token dup, cpeek, dup
	+literal $7f
	+token greater
	+qbranch_fwd nextword_pass
	+token dup
	+literal $ff				; Special case - if the byte at LFA is $ff link to the last word in core
	+token notequal
	+qbranch_fwd nextword_core
	+literal $7f
	+token and_op
	+literal 8
	+token lshift, over, oneplus, cpeek, or
nextword_pass:
	+token nip

	+token qdup
	+qbranch_fwd nextword_done
	+token sub, exit
nextword_done:
	+token drop, zero, exit
nextword_core:
	+token twodrop, drop
	+literal forth_system_n
	+token exit

; ==============================================================================
; Outer interpreter

+header ~state, ~state_n, "STATE"
	+code doconst
	+value _state

; A trick for check word to abort with the message
qcomp_abort:
	+token xabortq
+header ~qcomp, ~qcomp_n, "?COMP"
	+forth
	+token state, peek
	+qbranch qcomp_abort
	+token exit

qstack_abort:
	+token xabortq
+header ~qstack, ~qstack_n, "?STACK"
	+forth
	+token depth, dup, zerolt, swap
	+literal STACKLIMIT
	+token greater, or, zeroeq
	+qbranch qstack_abort
	+token exit

+header ~interpret, ~interpret_n, "INTERPRET"
	+forth
interpret_1:
	+token qstack, bl, word, dup, cpeek
	+qbranch_fwd interpret_done	; get the next word if any
	+token state, peek
	+qbranch_fwd interpret_int
	+token find, qdup
	+qbranch_fwd comp_num
	+token zerolt
	+qbranch_fwd comp_imm		; compiling now
	+token compilecomma
	+branch interpret_1		; regular word in compile mode
comp_imm:
	+token execute
	+branch interpret_1		; immediate word in compile mode
comp_num:
	+token number
	+branch interpret_1
interpret_int:
	+token find
	+qbranch_fwd int_num			; interpreting now
	+token execute
	+branch interpret_1		; any word in interpreter mode
int_num:
	+token number
	+branch interpret_1
interpret_done:
	+token drop, refill, zeroeq
	+qbranch interpret_1
	+token closesource, exit

+header ~closesource, ~closesource_n, "CLOSE-SOURCE"
	+forth
	+token sourceid
	+qbranch_fwd closesource_2						; nothing to do with console source
	+token sourceid, zerogt
	+qbranch_fwd closesource_1
	+token sourceid, closefile, drop, minusone
	+literal _ibufcount
	+token incpoke		; close file and release the buffer
closesource_1:
	+literal _source
	+token dup, peek, dup, peek, oneplus
	+token cells, add, swap, poke	; this will close the last frame
closesource_2:
	+token exit

; ==============================================================================
; Colon definition and related words
; (CREATE) takes cstr,n and creates a raw header (NFA+LFA)
+header ~xcreate, ~xcreate_n
	+code

; check if we have available tokens
	lda _hightoken+1
	cmp #>TOKEN_COUNT
	bcc + 

	+goforth
	+branch_fwd create_error
+:

; calculate contents for the new LFA
; Temporarily reloading _latest as it will be updated on the next step
; Special case to link to the core first
	lda _latest
	cmp #<forth_system_n
	bne +
	lda _latest+1
	cmp #>forth_system_n
	bne +
	lda #0
	sta _wscratch
	lda #$ff
	sta _wscratch+1
	bne ++
+
	lda _current
	asl
	tay
	lda _vocs,y
	sta _latest
	lda _vocs+1,y
	sta _latest+1

	ora _latest			; if _latest is NULL at this point, the LFA should be NULL as well, not a diff (TODO: validate)
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
	cmp #$ff
	beq xcreate_4
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

+error_message ~create_error
+header ~create, ~create_n, "CREATE"
	+forth
	+token here, bl, word, count, xcreate
	+literal JMP_INSTR
	+token ccomma
	+literal created
	+token comma, context, poke, exit

; DOES> is a weird beast. It generates code that will modify the execution of the
; last defined word to jump to the definition word. It is also quite non-portable as it generates a low level instruction
+header ~xcode, ~xcode_n		; (;CODE)
	+forth
	+token rfrom								; which is the address of the "call xdoes" instruction
	+token latest, count
	+literal NAMEMASK
	+token and_op, add, lfatocfa ;twoplus		; CFA of the last defined word
	+token oneplus ; PFA (!)
	+token poke, exit							; and this will actually exit the defining word

qdefer_abort:
	+token xabortq
+header ~qdefer, ~qdefer_n, "?DEFER"
	+forth
	+token dup, tobody, twominus, peek
	+literal dodefer
	+token equal
	+qbranch qdefer_abort
	+token exit

+header ~deferpeek, ~deferpeek_n, "DEFER@"
	+forth
	+token qdefer, tobody, peek, exit

+header ~deferpoke, ~deferpoke_n, "DEFER!"
	+forth
	+token qdefer, tobody, poke, exit

+header ~comppoke, ~comppoke_n
	+forth
	+token compile, lit, comma, compile, poke, exit

+error_message ~tick_error
+header ~tick, ~tick_n, "'"
	+forth
	+token bl, word, find
	+qbranch tick_error
	+token exit

+header ~btick, ~btick_n, "[']", IMM_FLAG
	+forth
	+token qcomp, tick
	+token compile, lit, comma
	+token exit

; This will get the next parameter, compile it to the current definition and skip
+header ~compile, ~compile_n, "COMPILE"
	+forth
	+token rfrom, dup, oneplus, swap, cpeek, dup
	+literal 16
	+token less
	+qbranch_fwd compile_lastbyte
	+token ccomma, dup, oneplus, swap, cpeek
compile_lastbyte:
	+token ccomma, tor, exit

+header ~compilecomma, ~compilecomma_n, "COMPILE,"
	+forth
	+token dup
	+literal 255
	+token greater
	+qbranch_fwd compilecomma_1
	+token dup
	+literal 8
	+token rshift, ccomma
compilecomma_1:
	+token ccomma, exit

+header ~bracket, ~bracket_n, "[", IMM_FLAG
	+forth
	+token qcomp, false, state, poke, exit

+header ~bracketx, ~bracketx_n, "]"
	+forth
	+token true, state, poke, exit

+header ~commaquote, ~commaquote_n, ",\""
	+forth
	+literal '"'
	+token parse, dup, ccomma, here
	+token over, allot, swap, cmove, exit

+header ~cquote, ~cquote_n, "C\"", IMM_FLAG
	+forth
	+token qcomp, compile, branch, fmark
	+token here, swap, commaquote, fresolve, compile, lit, comma
	+token exit


+header ~xabortq, ~xabortq_n, "(ABORT\")"
	+forth
	+token rat, count
	+literal NAMEMASK
	+token and_op, type, abort, exit

+header ~xquit, ~xquit_n
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


+header ~xforget, ~xforget_n	; xt -> (delete all words from this address)
	+forth
; Protect the core
	+token dup
	+literal forth_system
	+token greater
	+qbranch_fwd xforget_error
; Reset the search order to default
	+token minusone, set_order, zero
	+literal _current
	+token cpoke

; Set HERE to the NFA of the specified word. Keep that NFA for further comparisons
	+token dup, xttocfa, cfatolfa, lfatonfa
	+literal _here
	+token poke
; Set hightoken to the previous one and get that NFA for LATEST
	+token oneminus, dup
	+literal _hightoken
	+token poke, xttocfa, cfatolfa, lfatonfa
	
	+token dup
	+literal _latest
	+token poke

; Delete all vocabularies above HERE, except voc 0 (may be in high ROM)
	+literal _numvocs
	+token cpeek, one, xqdo
	+address xforget_vndone
xforget_vn:
	+token dup, i, cells
	+literal _vocsref
	+token add, peek, less
	+qbranch_fwd xforget_vnok
	+token i
	+literal _numvocs
	+token cpoke, leave
xforget_vnok:
	+token xloop
	+address xforget_vn
xforget_vndone:

; In all remaining vocabularies, remove all words above here
	+literal _numvocs
	+token cpeek, zero, xqdo
	+address xforget_wdone
xforget_wloop:
	+token i, cells
	+literal _vocs
	+token add, peek
xforget_nw:
	+token dup, tor, over, greater
	+qbranch_fwd xforget_nwok
	+token rfrom, nextword
	+branch xforget_nw
xforget_nwok:
	+token rfrom, i, cells
	+literal _vocs
	+token add, poke
	+token xloop
	+address xforget_wloop
xforget_wdone:

	+token drop, exit

xforget_error:
	+token xabortq
	+string "?FENCE"

+header ~addfield, ~addfield_n, "+FIELD"
	+forth
	+token create, over, comma, add, xcode
	!byte JSR_INSTR
	+address does
	+token peek, add, exit
			

; ==============================================================================
; These are non-standard, but they are used to implement control words,
; basically, that's how forward and backward references are done.
; Not exposing in core, but they will be documented in the toolkit

+header ~fmark, ~fmark_n	; >MARK
	+forth
	+token here, zero, comma, exit

+header ~fresolve, ~fresolve_n	; >RESOLVE
	+forth
	+token here, swap, poke, exit

+header ~rmark, ~rmark_n	; <MARK
	+forth
	+token here, exit

+header ~rresolve, ~rresolve_n	; <RESOLVE
	+forth
	+token comma, exit

; ==============================================================================
; Some nice to have words

+header ~spaces, ~spaces_n, "SPACES"
	+forth
spaces_1:
	+token dup, zerogt
	+qbranch_fwd spaces_2
	+token oneminus, space
	+branch spaces_1
spaces_2:
	+token drop, exit

; In optional String word set
+header ~cmove, ~cmove_n, "CMOVE"
	+code
	+dpop
	+stax _scratch
	+dpop
	+stax _wscratch
	+dpop
	+stax _rscratch
	
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

	jmp next

; In optional String word set
+header ~cmovex, ~cmovex_n, "CMOVE>"
	+code
	+dpop
	+stax _scratch
	+dpop
	+stax _wscratch
	+dpop
	+stax _rscratch
	
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
	jmp next

+header ~move, ~move_n, "MOVE"
	+forth
	+token rot, rot, twodup, less
	+qbranch_fwd move_1
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

+header ~smove, ~smove_n	; SMOVE
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
	and #$5f			; case insensitive
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
	bmi smove_11
	cmp #'Z'+1
	bpl smove_11
	stx _stemp
	+sub 'A'
	tax
	lda smove_subst,x
	ldx _stemp
	jmp smove_8
smove_11:
	lda (_rscratch),y	; reload to restore case
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
	cmp #$40
	bcc +
	and #$5f
+:
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

+header ~fill, ~fill_n, "FILL"
	+forth
	+token swap, tor, swap
fill_1:
	+token rfrom, qdup
	+qbranch_fwd fill_2
	+token oneminus, tor, twodup, cpoke, oneplus
	+branch fill_1
fill_2:
	+token drop, drop, exit


; The next two are non-standard but proposed for inclusion
+header ~place, ~place_n, "PLACE"
	+forth
	+token twodup, twotor, charplus, swap
	+token chars, move, tworfrom, cpoke, exit

+header ~plusplace, ~plusplace_n, "+PLACE"
	+forth
	+token dup, count, add, tor, twodup, cpeek
	+token add, swap, cpoke, rfrom, swap, move, exit

; ==============================================================================
; More words from the optional Double-Number word set

wlow = _scratch
whigh = _rscratch

+header ~dadd, ~dadd_n, "D+"
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


+header ~dlit, ~dlit_n
	+forth
	+token rat, twopeek, rfrom, twoplus, twoplus, tor, exit


+header ~compdpoke, ~compdpoke_n
	+forth
	+token compile, lit, comma, compile, twopoke, exit


;
; M*/ is an unusual word that uses three-cell numbers. It is possible to build it from the existing words
; To make it more clear, using some internal helpers:
; : t* ( ud,u -- ut) 2>r r@ m* 0 2r> m* d+ ;
; : t/ ( ut,u -- ud) dup >r um/mod r> swap >r um/mod nip r> ;
; : normsign ( d,n -- ud,u,n ) 2dup xor >r abs rot rot dabs rot r> ;
;
+header ~tmult, ~tmult_n
	+forth
	+token twotor, rat, ummult, zero, tworfrom, ummult, dadd, exit

+header ~tdiv, ~tdiv_n
	+forth
	+token dup, tor, ummod, rfrom, swap
	+token tor, ummod, nip, rfrom, exit

+header ~normsign, ~normsign_n
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
+header ~c64open, ~c64open_n
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
+header ~c64close, ~c64close_n
	+code
	+dpop
	jsr CLOSE
	jmp next

+header ~ro, ~ro_n, "R/O"
	+code doconst
	+value ro_v
ro_v:
	+string ",S,R"

+header ~openfile, ~openfile_n, "OPEN-FILE"
	+forth
	+token tor
	+literal of_1
	+token count
	+literal _fnamebuf
	+token place
	+literal _fnamebuf
	+token plusplace
	+token rfrom, count
	+literal _fnamebuf
	+token plusplace
	+literal _openfiles
	+token peek, freebit
	+token dup
	+qbranch_fwd of_2
	+literal 8
	+token over
	+literal _fnamebuf
	+token count, c64open, dup
	+literal _openfiles
	+token setbit, dup, zeroeq
of_2:
	+token exit
of_1:
	+string "O0:"

+header ~closefile, ~closefile_n, "CLOSE-FILE"
	+forth
	+token dup
	+literal _openfiles
	+token clearbit, c64close, zero, exit

; Cannot be implemented on C64
+header ~repositionfile, ~repositionfile_n, "REPOSITION-FILE"
	+forth
	+token drop, twodrop, minusone, exit

; Cannot be implemented on C64
+header ~fileposition, ~fileposition_n, "FILE-POSITION"
	+forth
	+token drop, zero, zero, minusone, exit

; A simplistic implementation to test for existence
+header ~filestatus, ~filestatus_n, "FILE-STATUS"
	+forth
	+token ro, openfile
	+qbranch_fwd filestatus_1
	+token true, exit
filestatus_1:
	+token closefile, zero, exit

+header ~setread, ~setread_n
	+code
	+dpop
	tax
	jsr CHKIN
	jmp next

+header ~xreadchar, ~xreadchar_n
	+code
	jsr CHRIN
	ldx #0
	and #$7F		; Ignore high bit (so Shift-Space is not a problem)
	cmp #10			; Do two substitutions: \n -> \r and \t -> ' '
	bne xreadchar_1
	lda #NEW_LINE
xreadchar_1:
	cmp #9
	bne xreadchar_2
	lda #32
xreadchar_2:
	+dpush
	jmp next

+header ~xreadbyte, ~xreadbyte_n
	+code
	jsr CHRIN
	ldx #0
	+dpush
	jmp next

+header ~iseof, ~iseof_n
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

+header ~readline, ~readline_n, "READ-LINE"
	+forth
	+token setread, swap, dup, rot, add, over					; c-addr, c-addr-limit, current
readline_1:
	+token twodup, swap, uless
	+qbranch_fwd readline_4				; buffer full?
readline_7:
	+token iseof, zeroeq
	+qbranch_fwd readline_2
	+token xreadchar, qdup
	+qbranch readline_7 ; EOF workaround
	+token dup
	+literal NEW_LINE
	+token notequal
	+qbranch_fwd readline_3		; end of line
	+token over, cpoke, oneplus
	+branch readline_1 
readline_2:
	+token nip, swap, sub, dup, zeroeq
	+qbranch_fwd readline_5
	+token false
	+branch_fwd readline_6
readline_3:
	+token drop
readline_4:
	+token nip, swap, sub
readline_5:
	+token true
readline_6:
	+token zero, setread, zero, exit

+header ~setwrite, ~setwrite_n
	+code
	+dpop
	tax
	jsr CHKOUT
	jmp next

xputchar = emit

; This can be implemented using KERNAL SAVE, but the corresponding READ-LINE cannot be implemented
; with KERNAL LOAD. Leaving it as is for now.
+header ~writefile, ~writefile_n, "WRITE-FILE"
	+forth
	+token setwrite
writefile_1:
	+token qdup
	+qbranch_fwd writefile_2
	+token swap, dup, cpeek, xputchar, oneplus, swap, oneminus
	+branch writefile_1
writefile_2:
	+token drop, zero, setwrite, zero, exit

+error_message ~includefile_error
+header ~includefile, ~includefile_n, "INCLUDE-FILE"
	+forth
	+literal _ibufcount
	+token peek
	+literal 7
	+token greater
	+qbranch_fwd includefile_1
	+branch includefile_error
includefile_1:
	+literal _source
	+token peek
	+token twominus, zero, over, poke	; two more entries to keep fileposition before the last refill
	+token twominus, zero, over, poke
	+token twominus, zero, over, poke
	+token twominus
	+literal _ibuf
	+literal _ibufcount
	+token peek
	+literal 100
	+token mult, add, over, poke
	+token twominus, zero, over, poke
	+token twominus, tuck, poke
	+token twominus
	+literal 6
	+token over, poke
	+literal _source
	+token poke
	+literal _ibufcount
	+token dup, peek, oneplus, swap, poke
	+token state, peek
	+qbranch_fwd includefile_2
	+token interpret
includefile_2:
	+token exit

+header ~included, ~included_n, "INCLUDED"
	+forth
	+token twodup, filestatus, nip
	+qbranch_fwd included_1
	+token twodrop, exit
included_1:
	+token twodup, here, tor, xcreate	; cr eate a dummy word with the same name as the included file
	+literal RTS_INSTR
	+token ccomma, compile, exit, rfrom, context, poke
	+token ro, openfile
	+qbranch_fwd included_2
	+token drop, exit
included_2:
	+token includefile, exit

+header ~required, ~required_n, "REQUIRED"
	+forth
	+token twodup, context, peek, find
	+qbranch_fwd required_1
	+token twodrop, exit
required_1:
	+token drop, included, exit

; ============================================================================
; Here lies an important boundary - all words above it are used in other core
; words, everythign below is unreferenced. The order is important, so smaller
; token values will fit in one byte making core smaller.
; The boundary is not very precise, there is some slack. Placing words too
; low may cause compile errors, placing too high is benign.
; ============================================================================
+check_token_range


+header ~bin, ~bin_n, "BIN"
	+forth
	+token exit		; taking the recommendation and handling all files as binary

+header ~wo, ~wo_n, "W/O"
	+code doconst
	+value wo_v
wo_v:
	+string ",S,W"

; This may not be supported on C64, making it identical to W/O
+header ~rw, ~rw_n, "R/W"
	+code doconst
	+value wo_v

; For C64 OPEN-FILE and CREATE-FILE are identical
+header ~createfile, ~createfile_n, "CREATE-FILE"
	+forth
	+token openfile, exit

; C64 equivalent: OPEN 1,8,15,"S0:Name":CLOSE 1
+header ~deletefile, ~deletefile_n, "DELETE-FILE"
	+forth
	+literal df_1
	+token count
	+literal _fnamebuf
	+token place
	+literal _fnamebuf
	+token plusplace
	+token one
	+literal 8
	+literal 15
	+literal _fnamebuf
	+token count, c64open
	+token one, notequal, one, c64close, exit
df_1:
	+string "S0:"

; C64 equivalent: OPEN 1,8,15,"R0:NewName=OldName":CLOSE 1
; Note that this is the only word that uses PAD
+header ~renamefile, ~renamefile_n, "RENAME-FILE"
	+forth
	+literal rf_1
	+token count
	+literal _fnamebuf
	+token place
	+literal _fnamebuf
	+token plusplace
	+literal rf_2
	+token count
	+literal _fnamebuf
	+token plusplace
	+literal _fnamebuf
	+token plusplace
	+token one
	+literal 8
	+literal 15
	+literal _fnamebuf
	+token count, c64open
	+token one, notequal, one, c64close, exit
rf_1:
	+string "R0:"
rf_2:
	+string "="

; Cannot be implemented on C64
+header ~resizefile, ~resizefile_n, "RESIZE-FILE"
	+forth
	+token drop, twodrop, minusone, exit

; Cannot be implemented on C64
+header ~filesize, ~filesize_n, "FILE-SIZE"
	+forth
	+token drop, zero, zero, minusone, exit

+header ~readfile, ~readfile_n, "READ-FILE"
	+forth
	+token setread, swap, dup, rot, add, over					; c-addr, c-addr-limit, current
readfile_1:
	+token twodup, swap, uless
	+qbranch_fwd readfile_3				; buffer full?
	+token iseof, zeroeq
	+qbranch_fwd readfile_2
	+token xreadbyte		; end of file?
	+token over, cpoke, oneplus
	+branch readfile_1 
readfile_2:
readfile_3:
	+token nip, swap, sub, zero, zero, setread, exit

+header ~writeline, ~writeline_n, "WRITE-LINE"
	+forth
	+token dup, tor, writefile, rfrom, setwrite
	+literal NEW_LINE
	+token xputchar, zero, setwrite, exit

; Not needed on C64
+header ~flushfile, ~flushfile_n, "FLUSH-FILE"
	+forth
	+token zero, exit

+header ~include, ~include_n, "INCLUDE"
	+forth
	+token parsename, included, exit

+header ~require, ~require_n, "REQUIRE"
	+forth
	+token parsename, required, exit


; ==============================================================================
; Some less commonly used (not used in core) math words
+header ~div, ~div_n, "/"
	+forth
	+token divmod, nip, exit

+header ~mod, ~mod_n, "MOD"
	+forth
	+token divmod, drop, exit

+header ~multdiv, ~multdiv_n, "*/"
	+forth
	+token multdivmod, nip, exit

;
; : fm/mod dup >r sm/rem
;          over dup 0<> swap 0< r@ 0< xor and
;          if 1- swap r> + swap else rdrop then ;
;

+header ~fmmod, ~fmmod_n, "FM/MOD"
	+forth
	+token dup, tor, smrem, over, dup, zerone
	+token swap, zerolt, rat, zerolt, xor, and_op
	+qbranch_fwd fmmod_1
	+token oneminus, swap, rfrom, add, swap
	+branch_fwd fmmod_2
fmmod_1:
	+token rdrop
fmmod_2:
	+token exit

+header ~max, ~max_n, "MAX"
	+forth
	+token twodup, less
	+qbranch_fwd max_1
	+token swap
max_1:
	+token drop, exit

+header ~min, ~min_n, "MIN"
	+forth
	+token twodup, greater
	+qbranch_fwd min_1
	+token swap
min_1:
	+token drop, exit

;
;	: within over - >r - r> u< ;
;
+header ~within, ~within_n, "WITHIN"
	+forth
	+token over, sub, tor, sub, rfrom, uless, exit

; ==============================================================================
; More words from the optional Double-Number word set

;
; : d= rot = >r = r> and ;
;
+header ~dequal, ~dequal_n, "D="
	+forth
	+token rot, equal, tor, equal, rfrom, and_op, exit

;
; : dmax 2over 2over d< if 2swap then 2drop ;
; : dmin 2over 2over d< invert if 2swap then 2drop ;
;

+header ~dmax, ~dmax_n, "DMAX"
	+forth
	+token twoover, twoover, dless
	+qbranch_fwd dmax_1
	+token twoswap
dmax_1:
	+token twodrop, exit

+header ~dmin, ~dmin_n, "DMIN"
	+forth
	+token twoover, twoover, dless, invert
	+qbranch_fwd dmin_1
	+token twoswap
dmin_1:
	+token twodrop, exit

;
; : d- dnegate d+ ;
; code d+
;
+header ~dsub, ~dsub_n, "D-"
	+forth
	+token dnegate, dadd, exit

+header ~dtwodiv, ~dtwodiv_n, "D2/"
	+forth
	+token dup, one, and_op
	+literal 15
	+token lshift, swap, twodiv, swap
	+token rot, twodiv, or, swap, exit

;
; : d2* 2dup d+ ;
;

+header ~dtwomul, ~dtwomul_n, "D2*"
	+forth
	+token twodup, dadd, exit

+header ~duless, ~duless_n, "DU<"
	+forth
	+token rot, twodup, equal
	+qbranch_fwd duless_1
	+token twodrop, uless, exit
duless_1:
	+token ugreater
	+qbranch_fwd duless_2
	+token twodrop, true, exit
duless_2:
	+token twodrop, false, exit

;
; : d0= or 0= ;
; : d0< nip 0< ;
; : d< rot > if 2drop true else < then ;
;
+header ~dzeroeq, ~dzeroeq_n, "D0="
	+forth
	+token or, zeroeq, exit

+header ~dzeroless, ~dzeroless_n, "D0<"
	+forth
	+token nip, zerolt, exit

+header ~dless, ~dless_n, "D<"
	+forth
	+token rot, twodup, equal
	+qbranch_fwd dless_1
	+token twodrop, uless, exit
dless_1:
	+token greater
	+qbranch_fwd dless_2
	+token twodrop, true, exit
dless_2:
	+token twodrop, false, exit

;
; : d>s drop ;
;
+header ~dtos, ~dtos_n, "D>S"
	+forth
	+token drop, exit

;
; : 2constant create , , does> 2@ ;
;

+header ~dconstant, ~dconstant_n, "2CONSTANT"
	+forth
	+token create, comma, comma, xcode
	!byte JSR_INSTR
	+address does
	+token twopeek, exit

;
; : 2lit r@ 2@ r> 2+ 2+ >r ; nonstandard
; : 2literal ?comp state @ if compile 2lit , , then ; immediate
;

+header ~dliteral, ~dliteral_n, "2LITERAL", IMM_FLAG
	+forth
	+token qcomp, state, peek
	+qbranch_fwd dliteral_1
	+token compile, dlit
	+token comma, comma
dliteral_1:
	+token exit

;
; : 2rot 5 roll 5 roll ;
;
+header ~drot, ~drot_n, "2ROT"
	+forth
	+literal 5
	+token roll
	+literal 5
	+token roll, exit

+header ~dvalue, ~dvalue_n, "2VALUE"
	+forth
	+token create
	+literal dovalue
	+token here, twominus, poke
	+literal dvalue_sem
	+token comma, comma, comma, exit
dvalue_sem:
	+value twopeek
	+value twopoke
	+value compdpoke

;
; : m*/ >r normsign r> swap >r >r t* r> t/ r> 0< if dnegate then ;
;

+header ~mmuldiv, ~mmuldiv_n, "M*/"
	+forth
	+token tor, normsign, rfrom, swap, tor, tor
	+token tmult, rfrom, tdiv, rfrom, zerolt
	+qbranch_fwd mmuldiv_1
	+token dnegate
mmuldiv_1:
	+token exit


; ==============================================================================
; Reset return stack, dispose of sources, close all open files, and reenter the system.
+header ~quit, ~quit_n, "QUIT"
	+forth
	+token xsst
	+token xquit ; this is an equivalent to ;CODE
	
; ==============================================================================
+header ~immediate, ~immediate_n, "IMMEDIATE"
	+forth
	+token latest, dup, cpeek
	+literal IMM_FLAG
	+token or, swap, cpoke, exit

; Note that while DOES> looks like high-level word its implementation is depended on the opcode for native CALL/JSR
+header ~doesx, ~doesx_n, "DOES>", IMM_FLAG
	+forth
	+token qcomp, compile, xcode
	+literal JSR_INSTR
	+token ccomma
	+literal does
	+token comma, exit	; compile (;CODE) followed by "call does_c"

; Note that colon will remove the word from the search order (to be restored by semicolon)
+header ~colon, ~colon_n, ":"
	+forth
	+token bl, word, count
	+token xcreate
	+literal RTS_INSTR
	+token ccomma
	+token bracketx, exit

; Words defined with :NONAME technically don't need to be linked in the vocabulary but if it is done that way RECURSE becomes harder
; to implement. It is easier just to link the word with emtpy name. In this implementation it has an unusual side effect that FIND
; will actually find the last :NONAME if searched for empty string and the test suite actually traps that (not an error though). But -
; standard does not specify it either way; and this is potentially useful.

;
; : :noname here 0 , latest , _latest ! here ' call , ] ;
;
 
+header ~colonnoname, ~colonnoname_n, ":NONAME"
	+forth
	+token zero, dup
	+token xcreate
	+literal RTS_INSTR
	+token ccomma
	+token bracketx
	+literal _hightoken
	+token peek, exit

+header ~bufferc, ~bufferc_n, "BUFFER:"
	+forth
	+token create, allot, exit

+header ~semicolon, ~semicolon_n, ";", IMM_FLAG
	+forth
	+token qcomp, compile, exit, bracket, latest, context, poke, exit

+header ~variable, ~variable_n, "VARIABLE"
	+forth
	+token create, zero, comma, exit

+header ~twovariable, ~twovariable_n, "2VARIABLE"
	+forth
	+token create, zero, dup, comma, comma, exit

+header ~constant, ~constant_n, "CONSTANT"
	+forth
	+token create, comma, xcode
	!byte JSR_INSTR
	+address does
	+token peek, exit

+header ~defer, ~defer_n, "DEFER"
	+forth
	+token create
	+literal dodefer
	+token here, twominus, poke		; note that we cannot use "compile exit" here as that will reserve only one byte,
	+literal exit					; and some tokens may need two
	+token comma, exit

+header ~actionof, ~actionof_n, "ACTION-OF", IMM_FLAG
	+forth
	+token state, peek
	+qbranch_fwd actionof_1
	+token btick, compile, deferpeek, exit
actionof_1:
	+token tick, deferpeek, exit

+header ~is, ~is_n, "IS", IMM_FLAG
	+forth
	+token state, peek
	+qbranch_fwd is_1
	+token btick, compile, deferpoke, exit
is_1:
	+token tick, deferpoke, exit

; "value" has a special structure: three tokens for read semantics,
; write semantics, and compile semantics, followed by the value itself

+header ~value, ~value_n, "VALUE"
	+forth
	+token create
	+literal dovalue
	+token here, twominus, poke
	+literal value_sem
	+token comma, comma, exit
value_sem:
	; Note that the parameter block uses "value" instead of "token" - this is
	; intentional as the size of token is not known
	+value peek
	+value poke
	+value comppoke

+error_message ~to_error
+header ~to, ~to_n, "TO", IMM_FLAG
	+forth
	+token bl, word, find
	+qbranch to_error
	+token tobody, dup, twominus, peek
	+literal dovalue
	+token equal
	+qbranch to_error
	+token dup, twoplus, swap, peek, state, peek
	+qbranch_fwd to_1
	+token twoplus
to_1:
	+token twoplus, peek, execute, exit

+header ~squote, ~squote_n, "S\"", IMM_FLAG
	+forth
	+token state, peek
	+qbranch_fwd squote_1
	+token cquote, compile, count, exit
squote_1:
	+literal '"'
	+token parse
	+literal _sbuf
	+literal _sflip
	+token cpeek
	+literal 100
	+token mult, add, swap, twotor, tworat, cmove
	+token tworfrom
	+literal _sflip
	+token dup, cpeek, one, xor, swap, cpoke, exit 

+header ~ssquote, ~ssquote_n, "S\\\"", IMM_FLAG
	+forth
	+token tib, ptrin, peek, add
	+literal _sbuf
	+literal _sflip
	+token cpeek
	+literal 100
	+token mult, add, numtib, peek
	+token ptrin, peek, sub, over, tor
	+token smove, swap, ptrin, incpoke
	+token rfrom, swap
	+literal _sflip
	+token dup, cpeek, one, xor, swap, cpoke
	+token state, peek
	+qbranch_fwd ssquote_1
	+token compile, branch, fmark
	+token here, two, pick, dup, ccomma, allot, swap, fresolve
	+token compile, lit, dup
	+token comma, compile, count, oneplus, swap, cmove 
ssquote_1:
	+token exit

+header ~dotquote, ~dotquote_n, ".\"", IMM_FLAG
	+forth
	+token qcomp, cquote, compile, count, compile, type, exit

+header ~char, ~char_n, "CHAR"
	+forth
	+token bl, word, charplus, cpeek, exit

+header ~bcharb, ~bcharb_n, "[CHAR]", IMM_FLAG
	+forth
	+token compile, blit, bl
	+token word, charplus, cpeek, ccomma, exit

+header ~abortq, ~abortq_n, "ABORT\"", IMM_FLAG
	+forth
	+token qcomp, compile, qbranch, fmark
	+token compile, xabortq, commaquote, fresolve, exit

; In optional Programming-Tools word set
+error_message ~forget_error:
+header ~forget, ~forget_n, "FORGET"
	+forth
	+token bl, word, find
	+qbranch forget_error
	+token xforget
	+token exit

+header ~marker, ~marker_n, "MARKER"
	+forth
	+token create
	+literal _hightoken
	+token peek, comma, xcode
	!byte JSR_INSTR
	+address does
	+token peek, xforget, exit

+header ~recurse, ~recurse_n, "RECURSE", IMM_FLAG
	+forth
	+token qcomp
	+literal _hightoken
	+token peek, compilecomma, exit

+header ~bcompile, ~bcompile_n, "[COMPILE]", IMM_FLAG
	+forth
	+token qcomp, tick, compilecomma, exit

; Somehow I've managed to get this to pass the tests but I still don't completely understand what
; it is supposed to do
+error_message ~postpone_error:
+header ~postpone, ~postpone_n, "POSTPONE", IMM_FLAG
	+forth
	+token qcomp, bl, word, find, qdup
	+qbranch postpone_error
	+token one, equal
	+qbranch_fwd postpone_1
	+token compilecomma, exit
postpone_1:
	+token compile, compile, compilecomma, exit

; This word behaves differently depending on compilation state - in compilation it
; will emit LIT followed by the value from the stack
+header ~literal, ~literal_n, "LITERAL", IMM_FLAG
	+forth
	+token qcomp, state, peek
	+qbranch_fwd literal_1
	+token compile, lit, comma
literal_1:
	+token exit

+header ~holds, ~holds_n, "HOLDS"
	+forth
holds_1:
	+token qdup
	+qbranch_fwd holds_2
	+token oneminus, twodup, add, cpeek, hold
	+branch holds_1
holds_2:
	+token drop, exit

+header ~dotr, ~dotr_n, ".R"
	+forth
	+token swap, stod, rot, ddotr, exit

+header ~udot, ~udot_n, "U."
	+forth
	+token zero, ddot, exit

+header ~udotr, ~udotr_n, "U.R"
	+forth
	+token zero, swap, ddotr, exit

+header ~pad, ~pad_n, "PAD"
	+code doconst
	+value _pad

+header ~erase, ~erase_n, "ERASE"
	+forth
	+token zero, fill, exit

+header ~sstring, ~sstring_n, "/STRING"
	+forth
	+token rot, over, add, rot, rot, sub, exit

+header ~blank, ~blank_n, "BLANK"
	+forth
	+token bl, fill, exit

+header ~sliteral, ~sliteral_n, "SLITERAL", IMM_FLAG
	+forth
	+token state, peek
	+qbranch_fwd sliteral_1
	+token compile, branch, fmark
	+token rot, rot
	+token dup, tor, here, dup, tor
	+token swap, dup, allot, cmove, fresolve
	+token compile, lit, rfrom
	+token comma, compile, lit, rfrom
	+token comma
sliteral_1:
	+token exit

+header ~qmark, ~qmark_n, "?"
	+forth
	+token peek, dot, exit

+header ~dots, ~dots_n, ".S"
	+forth
	+token depth
dots_1:
	+token qdup
	+qbranch_fwd dots_2
	+token dup, pick, dot, oneminus
	+branch dots_1
dots_2:
	+token exit

+header ~ahead, ~ahead_n, "AHEAD"
	+forth
	+token fmark, exit


cstr1 = _dtop
clen1 = _scratch
cstr2 = _rscratch
clen2 = _wscratch

; COMPARE became standard in the later versions of the language.
; In optional String word set
; This one still can be optimized further
; (caddr1, u1, caddr2, u2 -> n)
+header ~compare, ~compare_n, "COMPARE"
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

+header ~saveinput, ~saveinput_n, "SAVE-INPUT"
	+forth
	+literal _source
	+token peek, dup, tor, peek
saveinput_1:
	+token qdup
	+qbranch_fwd saveinput_2
	+token dup, twomult, rat, add, peek, swap, oneminus
	+branch saveinput_1
saveinput_2:
	+token rfrom, peek, exit

+header ~restoreinput, ~restoreinput_n, "RESTORE-INPUT"
	+forth
	+token over, sourceid, equal
	+qbranch_fwd restoreinput_3
	+token sourceid, zerogt
	+qbranch_fwd restoreinput_1
	+literal 6
	+token pick
	+literal 6
	+token pick, sourceid, repositionfile, refill, twodrop
restoreinput_1:
	+token qdup
	+qbranch_fwd restoreinput_2
	+token dup, roll, over, twomult
	+literal _source
	+token peek, add, poke, oneminus
	+branch restoreinput_1
restoreinput_2:
	+token false, exit
restoreinput_3:
	+token true, exit

+header ~evaluate, ~evaluate_n, "EVALUATE"
	+forth
	+literal _source
	+token peek
	+token twominus, tuck, poke
	+token twominus, tuck, poke
	+token twominus, zero, over, poke
	+token twominus, minusone, over, poke
	+token twominus
	+literal 4
	+token over, poke
	+literal _source
	+token poke
	+token interpret
	+token exit



; ==============================================================================

+header ~align, ~align_n, "ALIGN"
	+forth
	+token exit

+header ~aligned, ~aligned_n, "ALIGNED"
	+forth
	+token exit


; ==============================================================================
; Control words. All of these are immediate and don't do anything useful
; in interpreter mode. There should be no code calling to CFA of these words.
; To understand the concept behind these words look at the BEGIN/AGAIN pair -
; BEGIN ends up just putting RI on the stack and AGAIN compiles BRANCH to that RI.
; Forward references are a bit trickier but follow the same pattern.

+header ~begin, ~begin_n, "BEGIN", IMM_FLAG
	+forth
	+token qcomp, rmark, exit

+header ~until, ~until_n, "UNTIL", IMM_FLAG
	+forth
	+token qcomp, compile, qbranch, rresolve
	+token exit

+header ~again, ~again_n, "AGAIN", IMM_FLAG
	+forth
	+token qcomp, compile, branch, rresolve
	+token exit

+header ~if, ~if_n, "IF", IMM_FLAG
	+forth
	+token qcomp, compile, qbranch, fmark
	+token exit

+header ~then, ~then_n, "THEN", IMM_FLAG
	+forth
	+token qcomp, fresolve, exit

+header ~else, ~else_n, "ELSE", IMM_FLAG
	+forth
	+token qcomp, compile, branch, fmark
	+token swap, fresolve, exit

+header ~while, ~while_n, "WHILE", IMM_FLAG
	+forth
	+token qcomp, compile, qbranch, fmark
	+token swap, exit

+header ~repeat, ~repeat_n, "REPEAT", IMM_FLAG
	+forth
	+token qcomp, compile, branch, rresolve
	+token fresolve, exit

+header ~do, ~do_n, "DO", IMM_FLAG
	+forth
	+token qcomp, compile, xdo, fmark, rmark, exit

+header ~qdo, ~qdo_n, "?DO", IMM_FLAG
	+forth
	+token qcomp, compile, xqdo, fmark, rmark, exit

+header ~loop, ~loop_n, "LOOP", IMM_FLAG
	+forth
	+token qcomp, compile, xloop, rresolve, fresolve, exit

+header ~ploop, ~ploop_n, "+LOOP", IMM_FLAG
	+forth
	+token qcomp, compile, xploop, rresolve, fresolve, exit

+header ~unloop, ~unloop_n, "UNLOOP"
	+forth
	+token rfrom, rdrop, rdrop, rdrop, tor, exit

+header ~case, ~case_n, "CASE", IMM_FLAG
	+forth
	+token qcomp, depth, rfrom, swap, tor, tor, exit

+header ~of, ~of_n, "OF", IMM_FLAG
	+forth
	+token qcomp, compile, over, compile, equal, compile, qbranch
	+token fmark, compile, drop, exit

+header ~endof, ~endof_n, "ENDOF", IMM_FLAG
	+forth
	+token qcomp, compile, branch, fmark
	+token swap, fresolve, exit

+header ~endcase, ~endcase_n, "ENDCASE", IMM_FLAG
	+forth
	+token qcomp, compile, drop, depth
	+token rfrom, rfrom, swap, tor, sub
endcase_1:
	+token qdup
	+qbranch_fwd endcase_2
	+token oneminus, swap, fresolve
	+branch endcase_1
endcase_2:
	+token exit

;
; : ( source-id 0< if
;     begin ')' parse 2drop >in @ #tib @ = tib #tib @ + 1- c@ ')' = and
;     while refill invert if exit then again
;     else ')' parse 2drop then ; immediate
;

+header ~brace, ~brace_n, "(", IMM_FLAG
	+forth
	+token sourceid, zerogt
	+qbranch_fwd brace_2
brace_1:
	+literal ')'
	+token parse, twodrop, ptrin, peek, numtib, peek, equal
	+token tib, numtib, peek, add, oneminus, cpeek
	+literal ')'
	+token notequal, and_op
	+qbranch_fwd brace_3
	+token refill, invert
	+qbranch brace_1
	+token exit
brace_2:
	+literal ')'
	+token parse, twodrop
brace_3:
	+token exit

+header ~backslash, ~backslash_n, "\\", IMM_FLAG
	+forth
	+token zero, parse, twodrop, exit

+header ~dotbrace, ~dotbrace_n, ".(", IMM_FLAG
	+forth
	+literal ')'
	+token parse, type, exit


; ==============================================================================
; Small subset from the optional Facility word set

+header ~beginstructure, ~beginstructure_n, "BEGIN-STRUCTURE"
	+forth
	+token create, here, zero, zero, comma, xcode
	!byte JSR_INSTR
	+address does
	+token peek, exit

+header ~endstructure, ~endstructure_n, "END-STRUCTURE"
	+forth
	+token swap, poke, exit
				
+header ~field, ~field_n, "FIELD:"
	+forth
	+token two, addfield, exit

+header ~cfield, ~cfield_n, "CFIELD:"
	+forth
	+token one, addfield, exit

; ==============================================================================
; Per discussion on forth-standard.org, it appears that this word does not
; have to provide any additional information. Given the overall bad specs and
; high memory use for little purpose, shortwiring it
+header ~environmentq, ~environmentq_n, "ENVIRONMENT?"
	+code doconst
	+value VAL_FALSE

; In optional Programming-Tools word set
+header ~words, ~words_n, "WORDS"
	+forth
	+token get_order, zero, tuck, xqdo
	+address words_done
words_looporder:
	+token swap, cells
	+literal _vocs
	+token add, peek
words_loop:
	+token qdup
	+qbranch_fwd words_next
	+token dup, count
	+literal NAMEMASK
	+token and_op, qdup
	+qbranch_fwd words_noname
	+token type, space, swap, oneplus, swap
	+branch_fwd words_continue
words_noname:
	+token drop
words_continue:
	+token nextword
	+branch words_loop
words_next:
	+token xloop
	+address words_looporder
words_done:
	+token cr, dot
	+literal words_n
	+token count, type, exit

+header ~key, ~key_n, "KEY"
	+code
	jsr CHRIN			; TODO - this will echo the character, which contradicts the standard
	ldx #0
	+dpush
	jmp next

; ==============================================================================
; This word became standard in ANS Forth, part of optional Programming-Tools word set. Quit the interpreter.
; code bye
+header ~bye, ~bye_n, "BYE"
	+code
	pla
	pla
	rts

; Search-Order words
;

; : also get-order over swap 1+ set-order ;

+header ~also, ~also_n, "ALSO"
	+forth
	+token get_order, over, swap, oneplus, set_order, exit


+header ~definitions, ~definitions_n, "DEFINITIONS"
	+code
	lda _context
	sta _current
	jmp next

+header ~get_current, ~get_current_n, "GET-CURRENT"
	+forth
	+literal _current
	+token cpeek, exit

; : only -1 set-order ;

+header ~only, ~only_n, "ONLY"
	+forth
	+token minusone, set_order, exit

+header ~order, ~order_n, "ORDER"
	+forth
	+token get_order, zero, xqdo
	+address order_done
order_loop:
	+token dup, cells
	+literal _vocsref
	+token add, peek, count
	+literal NAMEMASK
	+token and_op, type
	+literal '*'
	+token emit
	+token dot, xloop
	+address order_loop
order_done:
	+token exit

; : previous get-order nip 1- set-order ;

+header ~previous, ~previous_n, "PREVIOUS"
	+forth
	+token get_order, nip, oneminus, set_order, exit

+header ~set_current, ~set_current_n, "SET-CURRENT"
	+forth
	+literal _current
	+token cpoke, exit

+header ~wordlist, ~wordlist_n, "WORDLIST"
	+forth
	+token here, zero, dup, xwordlist, swap, context, poke, exit

+header ~forth, ~forth_n, "FORTH"
	+forth
	+token get_order, nip, zero		; FORTH-WORDLIST wid is 0
	+token swap, set_order, exit

+header ~forth_wordlist, ~forth_wordlist_n, "FORTH-WORDLIST"
	+code doconst
	+value 0

; ==============================================================================
; The main system loop. This has to be the last word in the core

+header ~forth_system, ~forth_system_n
	+forth
forth_system_c:
	+literal banner_text
	+token count, type, cr
	+token decimal, false, state, poke, xsst
; Register the root Forth dictionary
; One wid with the value 0; one voc with head pointing at the NFA of the last word
; Current is also at 0
	+token one, dup
	+literal _numorder
	+token cpoke
	+literal _numvocs
	+token cpoke

	+token zero, dup
	+literal _context
	+token cpoke
	+literal _current
	+token cpoke

	+literal forth_system_n
	+literal _vocs
	+token poke
	+literal forth_wordlist_n
	+literal _vocsref
	+token poke
;
	+literal autorun
	+token count, included
	+branch_fwd forth_system_1
forth_system_r:
	+token decimal, false, state, poke, xsst
forth_system_1:
	+token interpret
	+branch forth_system_1
banner_text:
	+string "FORTH TX16 1.1"
autorun:
	+string "AUTORUN.FTH"

; ==============================================================================

end_of_image:

!symbollist "symbols.txt"
