; BINSRCH
; searches sorted table of 16-bit values (table must be aligned
;     on two-byte boundary).

; prior to entry,
;     .low should point to first entry in the table.
;     .high should point to last entry in the table.
;     .value should contain the search target.

; on exit,
;     if carry set, value was found, and .mid contains pointer.
;     if carry clear, value not found, and .low points to where
;          target value should be inserted.


; zero page pointers

.low      = $10
.high     = $12
.mid      = $14
.value    = $16
.size     = 2                      ; element size (must be power of 2)


BINSRCH

.loop
          SEC                      ; calculate high - low
          LDA .high
          SBC .low
          TAX                      ; preserve LSB of difference in X
          LDA .high+1
          SBC .low+1
          BCC .done                ; if low > high, not found
          LSR                      ; calculate (high - low) / 2
          TAY
          TXA
          ROR                      ; carry cleared because multiple of 2
          AND #$100 - .size        ; align to element size
          ADC .low                 ; mid = low + ((high - low) / 2)
          STA .mid
          TYA
          ADC .low+1
          STA .mid+1
          LDA .value+1             ; load target value MSB
          LDY #1                   ; load index to MSB
          CMP (.mid),Y             ; compare MSB
          BEQ .chklsb
          BCC .modhigh             ; A[mid] > value
.modlow                            ; A[mid] < value
          LDA .mid                 ; low = mid + element size
          ADC #.size - 1           ; carry always set
          STA .low
          LDA .mid+1
          ADC #0
          STA .low+1
          JMP .loop
.chklsb
          LDA .value               ; load target value LSB
          DEY                      ; set index to LSB
          CMP (.mid),Y             ; compare LSB
          BEQ .done
          BCS .modlow              ; A[mid] < value
.modhigh                           ; A[mid] > value
          LDA .mid                 ; high = mid - element size
          SBC #.size - 1           ; carry always clear
          STA .high
          LDA .mid+1
          SBC #0
          STA .high+1
          JMP .loop
.done
          RTS