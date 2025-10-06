fileio_module_start = *

; Reserving zero page locations
; With some care could probably reuse regular scratches, but it may interfere with code being read and interpreted
+zpbyte ~completion
+zpword ~rc_scratch

; Just make sure we are not taking too much
+zero_page_end $90

; File handle is used as a source id and that is not expected to be 0. However, the stream index is 0 to 7.
; To make it easier, open increments the value before returning and all other calls decrement it of use
; _streamid-1 to the same effect

; Set all streamid to #255 thus marking all buffers available
f256buffersinit:
	ldy #7
	lda #255
-:
	sta _streamid,y
	dey
	bpl -
	stz _drive
	rts

; Common operations for many kernel calls. Checks for generic error and sets C if any occured.
; It will wait until an error or an event in 'completion' occurs.
!zone waitforcompletion
waitforcompletion:
	bcs error_return
-:
	jsr kernel_Yield
	jsr kernel_NextEvent
	bcs -
	lda event_buffer+off_event_type
	cmp completion
	beq .return
	cmp #kernel_event_file_ERROR
	beq error_return
	cmp #kernel_event_file_EOF
	beq error_return
	cmp #kernel_event_file_NOT_FOUND
	bne -
error_return:
	sec
	rts
.return:
	clc
	rts

; Open file with name in X:A, length in Y, open mode in _scratch. Return the buffer id in A and status in C
f256open:
	+stax kernel_args_file_open_fname
	sty kernel_args_file_open_fname_len

	; find an available buffer and put it in Y
	ldy #7
-:
	lda _streamid,y
	cmp #255
	beq +
	dey
	bmi error_return
	bra -
	
+:
	lda _scratch
	sta kernel_args_file_open_mode
	lda _drive
	sta kernel_args_file_open_drive
	sty kernel_args_file_open_cookie
	lda #kernel_event_file_OPENED
	sta completion
	jsr kernel_File_Open
	
	jsr waitforcompletion
	bcs open_complete

	ldy kernel_args_file_open_cookie
	lda #1				; this will appear as a valid stream at exhausted state, triggers a refill on the first read
	sta _streamptr,y
	sta _streamload,y
	lda event_buffer+off_event_file_stream
	sta _streamid,y
	iny
	tya
;	clc		; not needed as no instructions since waitforcompletion can change C

open_complete:
	rts


; Close file with buffer id in Y
f256close:
	lda _streamid-1,y
	cmp #255
	beq +
	phy
	sta kernel_args_file_close_stream
	jsr kernel_File_Close
	; According to the docs, no need to check for CLOSED
	ply
	lda #255
	sta _streamid-1,y
+:
	rts

; Make sure all files are closed
close_open_files:
	ldy #8
-:
	jsr f256close
	dey
	bne -
	stz _drive
	rts

; Refill input stream buffer. Buffer index in Y. Return the status in C
f256refill:
	dey
	lda #0
	sta _streamload,y
	sta _streamptr,y
	lda _streamid,y
	
	phy ; microkernel doc states that Y should be preserved, but it does not always seem to be the case
	
	sta kernel_args_file_read_stream
	lda #64
	sta kernel_args_file_read_buflen
	lda #kernel_event_file_DATA
	sta completion
	jsr kernel_File_Read

	jsr waitforcompletion
	bcs refill_complete

	lda event_buffer+off_event_file_data_read
	sta kernel_args_recv_buflen

	ply
	sta _streamload,y

	; load to streambuffer+64*y. 6502 is not great at shifts so using
	; a trick y<<6 = (y<<8)>>2
	sty kernel_args_recv_buf+1
	stz kernel_args_recv_buf
	lsr kernel_args_recv_buf+1
	ror kernel_args_recv_buf
	lsr kernel_args_recv_buf+1
	ror kernel_args_recv_buf
	clc
	lda kernel_args_recv_buf
	adc #<_streambuffer
	sta kernel_args_recv_buf
	lda kernel_args_recv_buf+1
	adc #>_streambuffer
	sta kernel_args_recv_buf+1
	jsr kernel_ReadData

	clc
	rts
	
refill_complete:	
	ply
	rts
	
; Read one byte (character) from the stream. Buffer ID in Y, returns the char in A, status in C
f256readchar:
	lda _streamload-1,y	; if the buffer is completely empty, it is either EOF or error, done with the stream
	bne +
-:
	sec
	rts
+:
	cmp _streamptr-1,y	; if pointer reached the load, need to refill the buffer if possible
	bne +
	
	phy
	jsr f256refill
	ply
	bcs -
	bra f256readchar
	
+:
	dey					; to 0-based!
	sty rc_scratch+1
	stz rc_scratch
	lsr rc_scratch+1
	ror rc_scratch
	lsr rc_scratch+1
	ror rc_scratch
	
	clc
	lda rc_scratch
	adc #<_streambuffer
	sta rc_scratch
	lda rc_scratch+1
	adc #>_streambuffer
	sta rc_scratch+1
	
	lda _streamptr,y
	pha
	inc
	sta _streamptr,y
	ply
	lda (rc_scratch),y
	clc
	rts
	
; Write block up to 64 byte (IEC restriction). X:A is the address of the block, Y length, stream id in _scratch
; Returns C set on error
f256write:
	+stax kernel_args_file_write_buf
	sty kernel_args_file_write_buflen
	ldy _scratch
	lda _streamid-1,y
	sta kernel_args_file_write_stream
	lda #kernel_event_file_WROTE
	sta completion
	jsr kernel_File_Write

	jmp waitforcompletion
	
; !warn "fileio_f256 module compiled to ", *-fileio_module_start, " bytes"
