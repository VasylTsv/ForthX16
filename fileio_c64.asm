; Full equivalent to C64 OPEN, not exposed to dictionary yet
; Note that it requires 5 stack parameters as the string is enchoded as (c_addr,u)
; Top of data stack will have filenum or 0 on error
+header ~c64open, ~c64open_n
	+code
	+dpop
	; Note that A is set exactly to what we need there
	ldx _dtop
	ldy _dtop+1
	jsr SETNAM
	+dpop
	lda _dtop
	pha
	+dpop
	lda _dtop
	pha
	+dpop
	pla
	tax
	pla
	tay
	lda _dtop
	pha
	jsr SETLFS
	jsr OPEN
	pla
	ldx #0
	jmp next

; Corresponding equivalent to CLOSE
+header ~c64close, ~c64close_n
	+code
	+dpop
	jsr CLOSE
	jmp next

; This is used to check the drive status which is a rather
; complex process on C64. Ignoring responses starting with 0 (no responses start with 1)
; Note that this would hang if there is no drive attached, requiring an extra check for drive
; on startup
+header ~c64iostatus, ~c64iostatus_n
	+code
	lda _nodrive
	bne +
	ldx #15
	jsr CHKIN
	jsr CHRIN
	pha
-:
	jsr CHRIN
	cmp #NEW_LINE
	bne -
	jsr CLRCHN
	pla
	cmp #'2'
	bcs +
	lda #0
+:
	tax
	jmp dpush_and_next

; This will actually report other I/O errors, but this should be fine
+header ~c64iseof, ~c64iseof_n
	+code
	jsr READST
	tax
	jmp dpush_and_next

+header ~prepfname, ~prepfname_n
	+forth
	+token count
	+literal _fnamebuf
	+token place
	+literal _fnamebuf
	+token plusplace
	+token exit

; Set/unset channel for read and write, these need to follow after each other to
; save on common code
+header ~setread, ~setread_n
	+code
	+dpop
	tax
	beq +
	jsr CHKIN
	jmp next

+header ~setwrite, ~setwrite_n
	+code
	+dpop
	tax
	beq +
	jsr CHKOUT
	jmp next
+:
	jsr CLRCHN
	jmp next

+header ~xreadchar, ~xreadchar_n
	+code
	jsr CHRIN
	ldx #0
	and #$7F		; Ignore high bit (so Shift-Space is not a problem)
	cmp #10			; Do two substitutions: \n -> \r and \t -> ' ' (actually, everything until \t)
	bne +
	lda #NEW_LINE
+:
	bcs +
	lda #32
+:
	jmp dpush_and_next

+header ~xreadcharchecked, ~xreadcharchecked_n
	+forth
	+token xreadchar, dup
	+literal NEW_LINE
	+token equal, exit

+header ~xreadbyte, ~xreadbyte_n
	+code
	jsr CHRIN
	ldx #0
	+dpush
	lda #0
	tax
	jmp dpush_and_next

; This is the common code of READ-LINE and READ-FILE. There are only two differences between them -
; that READ-LINE stops on NL and there is one extra return value. Making internal reader vectorized
; takes care of the first issue, the second one is trivial.

_bytereader = _scratch_1

+header ~readgen, ~readgen_n
	+forth
	+literal _bytereader
	+token cpoke
	+token dup
	+literal _eoffiles
	+token getbit
	+qbranch_fwd readgen_good
	+token twodrop, drop, zero, false, zero, exit	; remove three parameters and put three 0s
readgen_good:
	+token dup, tor, setread, swap, dup, rot, add, over	; c-addr, c-addr-limit, current; fileid is accessible on the rstack
readgen_loop:
	+token twodup, swap, uless			; compare current < c-addr-limit
	+qbranch_fwd readgen_done			; finished?
	+literal _bytereader
	+token cpeek, execute
	+token tor, over, cpoke, oneplus, rfrom	; append to buffer
	+token c64iseof, twodup, or	; top of the stack: NL, EOF, true if either is set
	+qbranch_fwd readgen_continue
	+qbranch_fwd readgen_noteof
	+token rat
	+literal _eoffiles
	+token setbit						; just set the flag, it will block the following attempts
readgen_noteof:
	+qbranch_fwd readgen_done
	+token oneminus						; NL is not supposed to be included in the count
readgen_done:
	+token nip, swap, sub				; drop c-addr-limit; current - c-addr
	+token zero, setread
	+token rdrop, true, c64iostatus, exit
readgen_continue:
	+token twodrop
	+branch readgen_loop					; remove both NL and EOF flags and proceed to the next char

; Close file handles 3-14, this is used in ABORT to reset the error state
close_open_files:
	lda #3
	sta _scratch
-:
	lda _scratch
	cmp #15
	beq +
	jsr CLOSE
	inc _scratch
	bne -
+:
	lda #7		; protect channels 0-2 and 15
	ldx #128
	+stax _openfiles
	rts

