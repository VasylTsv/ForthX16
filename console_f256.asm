!zone console

mmu_ctrl = $00
io_ctrl = $01

; Screen geometry is hardcoded to 80x60 for now. This is not very hard to make more
; flexible except for setpos which has hardcoded multiply by 80.
SCREEN_SIZE_X = 80
SCREEN_SIZE_Y = 60


; Reserving zero page locations
+zpword ~text_coord
+zpbyte ~text_attrib
; used in this module (not that gets can call puts and then in turn call scroll_line)
; - by cls and scroll_line
+zpword ~scroll_src
+zpword ~scroll_dest
; - by puts/putc
+zpword ~char_src
+zpword ~char_dest
+zpbyte ~char_count
; - by gets
+zpword ~gets_buffer
+zpbyte ~gets_limit
+zpbyte ~gets_current
+zpbyte ~gets_tmp

; Just make sure we are not taking too much
+zero_page_end $90


; Cursor control registers (ref. pg. 24)
CCR = $d010
CCH = $d012
CURX = $d014
CURY = $d016


;==============================================================================
; init_console - has to be called at the very beginning to set up the event
; handling (required for input).
; set_mode - is exposed as an extra entry, but right now it only handles one
; single text mode

!zone init_console
init_console:
	lda #<event_buffer
	sta kernel_args_events_dest
	lda #>event_buffer
	sta kernel_args_events_dest+1
	
	lda #$40
	sta text_attrib

; For now set 80x60 text (ref pg.22)
set_mode:
	stz io_ctrl
	lda #1	; TEXT and nothing else
	sta $d000
	lda #$10 ; 60Hz (80x60, opaque bkg)
	sta $d001
	rts

; event_t is 8 bytes although many events are smaller
event_buffer !byte 0,0,0,0,0,0,0,0

;==============================================================================
; cls - clear the screen contents with the current text colors

!zone cls
cls:
	ldx     #3
	lda     text_attrib
	jsr     +

	ldx     #2
	lda     #' '

+:			; plane index in X, value in A
	stx io_ctrl

	ldx #<$c000
	stx scroll_dest+0
	stx text_coord
	ldx #>$c000
	stx scroll_dest+1
	stx text_coord+1

	ldx #SCREEN_SIZE_Y	; Note - fill one extra line below the screen to make scrolling simpler
.loop:
	ldy #(SCREEN_SIZE_X-1)
-:
	sta (scroll_dest),y
	dey
	bpl -
	clc
	pha
	lda scroll_dest
	adc #SCREEN_SIZE_X
	sta scroll_dest
	bcc +
	inc scroll_dest+1
+:
	pla
	dex
	bpl .loop
	
	rts

;==============================================================================
; scroll_line - scroll the text screen up one line
; Note that scroll_line moves text_coord up and can potentially move outside of
; the screen. This is okay in the context of this implementation, but it also
; means that it needs to be followed by setpos for safety in general context.
	
!zone scroll_line
scroll_line:
	sec
	lda text_coord
	sbc #SCREEN_SIZE_X
	sta text_coord
	bcs +
	dec text_coord+1
+:

	lda #3
	jsr +
	lda #2

+:
	sta io_ctrl

	ldx #<$c000
	stx scroll_src+0
	ldx #>$c000
	stx scroll_src+1
	
	ldx #(SCREEN_SIZE_Y-1)
.loop:
	clc
	lda scroll_src
	sta scroll_dest
	adc #SCREEN_SIZE_X
	sta scroll_src
	lda scroll_src+1
	sta scroll_dest+1
	adc #0
	sta scroll_src+1
	ldy #(SCREEN_SIZE_X-1)
-:
	lda (scroll_src),y
	sta (scroll_dest),y
	dey
	bpl -
	dex
	bpl .loop
	rts

;==============================================================================
; puts - prints a string of specifie length using current text colors
; X:A - address of the string, Y - string length
; Note that this code is optimized for string output vs the single character
; output. Not sure if it is the best approach yet. It may also be a slightly
; larger because of that.

!zone puts
puts:
	cpy #0
	beq .done
	sta char_src
	stx char_src+1
	sty char_count
	clc
	lda text_coord
	sta char_dest
	adc char_count
	sta text_coord
	lda text_coord+1
	sta char_dest+1
	adc #0
	sta text_coord+1

-:
; compare text_coord against the end of the screen
	lda text_coord+1
	cmp #>($c000+SCREEN_SIZE_X*SCREEN_SIZE_Y)
	bcc .proceed
	bne +
	lda text_coord
	cmp #<($c000+SCREEN_SIZE_X*SCREEN_SIZE_Y)
	bcc .proceed
+:
	sec
	lda char_dest
	sbc #SCREEN_SIZE_X
	sta char_dest
	bcs +
	dec char_dest+1
+:
	jsr scroll_line
	bra -

.proceed:
	ldy char_count
	
	dey
	phy
	lda #3
	sta io_ctrl
	lda text_attrib
-:
	sta (char_dest),y
	dey
	bpl -
	ply
	lda #2
	sta io_ctrl
-:
	lda (char_src),y
	sta (char_dest),y
	dey
	bpl -

.done:
	rts

;==============================================================================
; setpos - set output position to the specified screen coordinates
; coordinates are passed in X and Y

!zone setpos
setpos:
	cpx #SCREEN_SIZE_X
	bcc +
	ldx #(SCREEN_SIZE_X-1)
+:
	cpy #SCREEN_SIZE_Y
	bcc +
	ldy #(SCREEN_SIZE_Y-1)
+:
	
	stx text_coord
	ldx #>$c000
	stx text_coord+1
	sty scroll_src
	stz scroll_src+1
	asl scroll_src
	rol scroll_src+1
	asl scroll_src
	rol scroll_src+1
	jsr +	; this call will add Y<<2 to pos, and then fall in to add Y<<4 thus multiplying by 8
+:	
	asl scroll_src
	rol scroll_src+1
	asl scroll_src
	rol scroll_src+1
	lda text_coord
	adc scroll_src
	sta text_coord
	lda text_coord+1
	adc scroll_src+1
	sta text_coord+1
	rts

;==============================================================================
; getpos - return the current screen coordinates in X and Y

!zone getpos
getpos:
	lda text_coord
	sta scroll_src
	lda text_coord+1
	sta scroll_src+1
	ldy #0
-:
	lda scroll_src+1
	cmp #>($c000+SCREEN_SIZE_X)
	bcc getpos_first_line
	bne +
	lda scroll_src
	cmp #<($c000+SCREEN_SIZE_X)
	bcc getpos_first_line
+:
	iny
	sec
	lda scroll_src
	sbc #SCREEN_SIZE_X
	sta scroll_src
	bcs +
	dec scroll_src+1
+:
	bra -
	
getpos_first_line:
	ldx scroll_src
	rts

;==============================================================================
; newline - move the screen position to the beginning of the next line, scroll
; if necessary

!zone newline
newline:
	jsr getpos
	cpy #(SCREEN_SIZE_Y-1)
	bcc +
	jsr scroll_line
	jsr getpos
+:	
	iny
	ldx #0
	jmp setpos

;==============================================================================
; getch - get the next ASCII key without echo. Returns 0 for non-ASCII keys

!zone getch
getch:
.wait:
    lda kernel_args_events_pending
    bpl .wait

    jsr kernel_NextEvent
    bcs .wait

    lda event_buffer+off_event_type    
    cmp #kernel_event_key_PRESSED
	bne .wait

    lda event_buffer+off_event_key_flags 
    and #event_key_META
    beq .ascii
	lda #0
    rts
.ascii:
    lda event_buffer+off_event_key_ascii
    rts	

;==============================================================================
; gets - get string
; x:a - address of the buffer for the string, y - buffer size
; returns actual number of characters in y

!zone gets
gets:
	sta gets_buffer
	stx gets_buffer+1
	sty gets_limit
	stz gets_current
	
; Setup the cursor
	stz io_ctrl
	lda #3
	sta CCR
	lda #$d6	; Hatch block, same as in SuperBasic
	sta CCH
	stz CURX+1	; Cursor coordinates are 8-bit
	stz CURY+1
	
.loop:
; Update the cursor
	jsr getpos
	stz io_ctrl
	stx CURX
	sty CURY
	
; Get one key and handle special keys
	jsr getch
;	cmp #0
	beq .loop
	cmp #13			; CR returns the result
	beq .done
	cmp #8			; Backspace allows to delete the last character
	beq .bsp
	
; Any space left in buffer?
	ldy gets_current
	cpy gets_limit
	bcs .loop
	
; Update the buffer and reflect the character on the screen
	sta (gets_buffer),y
	jsr putc
	inc gets_current
	
; Continue
	bra .loop
	
.bsp:
; Make sure it is not the first character
	lda gets_current
	beq .loop

; Step back in the buffer and on the screen pos
	dec gets_current
	lda text_coord
	bne +
	dec text_coord+1
+:	dec text_coord

; Replace the last character on the screen with space
	lda #2
	sta io_ctrl
	ldy #0
	lda #' '
	sta (text_coord),y
	
; Continue
	bra .loop
	
.done:
; Hide the cursor
	stz io_ctrl
	stz CCR
; Done
	ldy gets_current
	rts

;==============================================================================
; putc - output one character in A

!zone putc
putc:
	sta gets_tmp
	lda #<gets_tmp
	ldx #>gets_tmp
	ldy #1
	jmp puts

;==============================================================================
