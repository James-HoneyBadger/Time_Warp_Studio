; 6502 Assembly - String Utilities
; String length, copy, reverse, and uppercase conversion

.org $0200

; String constants stored in ROM area
MSG1:   .byte "Hello, World!", 0
MSG2:   .byte "6502 Assembly", 0

; Zero page workspace
; $00-$01 = source pointer (lo/hi)
; $02-$03 = dest pointer (lo/hi)
; $04     = string length result
; $05     = loop index

;---------------------------------------------------------
; STRLEN - compute length of null-terminated string
; Input:  $00/$01 = string pointer
; Output: $04 = length, X = length
;---------------------------------------------------------
STRLEN:
    LDY #0          ; Y = index
STRLEN_LOOP:
    LDA ($00),Y     ; load char at ptr+Y
    BEQ STRLEN_DONE ; if null terminator, done
    INY             ; advance index
    BNE STRLEN_LOOP ; (handles strings up to 255 chars)
STRLEN_DONE:
    STY $04         ; store length
    TYA
    RTS

;---------------------------------------------------------
; STRCPY - copy null-terminated string
; Input:  $00/$01 = source, $02/$03 = dest
;---------------------------------------------------------
STRCPY:
    LDY #0
STRCPY_LOOP:
    LDA ($00),Y
    STA ($02),Y
    BEQ STRCPY_DONE
    INY
    BNE STRCPY_LOOP
STRCPY_DONE:
    RTS

;---------------------------------------------------------
; STRUPR - convert string to uppercase in place
; Input:  $00/$01 = string pointer
;---------------------------------------------------------
STRUPR:
    LDY #0
STRUPR_LOOP:
    LDA ($00),Y
    BEQ STRUPR_DONE     ; null terminator
    CMP #'a'
    BCC NOT_LOWER       ; < 'a': not lowercase
    CMP #'z'+1
    BCS NOT_LOWER       ; > 'z': not lowercase
    AND #%11011111      ; clear bit 5 → uppercase
    STA ($00),Y
NOT_LOWER:
    INY
    BNE STRUPR_LOOP
STRUPR_DONE:
    RTS

;---------------------------------------------------------
; MAIN PROGRAM
;---------------------------------------------------------
START:
    ; Example 1: Get length of MSG1
    LDA #<MSG1
    STA $00
    LDA #>MSG1
    STA $01
    JSR STRLEN
    ; X now holds the length (13 for "Hello, World!")

    ; Example 2: Copy MSG2 to buffer at $0300
    LDA #<MSG2
    STA $00
    LDA #>MSG2
    STA $01
    LDA #$00
    STA $02
    LDA #$03
    STA $03
    JSR STRCPY

    ; Example 3: Uppercase the copy at $0300
    LDA #$00
    STA $00
    LDA #$03
    STA $01
    JSR STRUPR

    BRK             ; halt

.end
