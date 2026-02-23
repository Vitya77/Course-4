; data.asm - Data directives: DB, DW, DD, EQU, TIMES
; Demonstrates data definition and constants

section .data

; Byte data (DB)
BYTE_VAL:     DB 42                ; Single byte
HEX_BYTE:     DB 0FFh              ; Hex byte
BYTE_LIST:    DB 1, 2, 3, 4, 5    ; Multiple bytes
HELLO_MSG:    DB 'Hello, World!', 0 ; Null-terminated string
NEWLINE:      DB 10                 ; Linefeed character
ESCAPE_STR:   DB "Line1\nLine2", 0  ; String with escape

; Word data (DW) - 16-bit
WORD_VAL:     DW 1234h             ; Single word
WORD_LIST:    DW 100, 200, 300     ; Multiple words
MAX_SHORT:    DW 0FFFFh            ; Maximum unsigned word

; Double word data (DD) - 32-bit
DWORD_VAL:    DD 12345678h         ; Single dword
DWORD_LIST:   DD 1000, 2000, 3000  ; Multiple dwords
PTR_VAL:      DD BYTE_VAL          ; Address (pointer) of label

; Constants (EQU) - no memory allocated
SYS_EXIT:     EQU 1
SYS_WRITE:    EQU 4
STDOUT:       EQU 1
BUFFER_SIZE:  EQU 256
ARRAY_LEN:    EQU 10
CALCULATED:   EQU BUFFER_SIZE * 2 + 1

; TIMES directive - repeat
ZEROS:        TIMES 8 DB 0         ; 8 zero bytes
PATTERN:      TIMES 4 DW 0AAAAh    ; 4 copies of 0AAAAh
PADDING:      TIMES 16 DB 90h      ; 16 NOP bytes

section .bss

; Uninitialized data
RESB_BUF:     RESB 64              ; 64 bytes reserved
RESW_BUF:     RESW 32              ; 32 words reserved
RESD_BUF:     RESD 16              ; 16 dwords reserved

section .text

    ; Using EQU constants
    MOV EAX, SYS_WRITE
    MOV EBX, STDOUT
    MOV ECX, BUFFER_SIZE
    MOV EDX, ARRAY_LEN

    ; Access defined data
    MOV AL, [BYTE_VAL]
    MOV BX, [WORD_VAL]
    MOV ECX, [DWORD_VAL]

    ; Load address of string
    LEA ESI, [HELLO_MSG]

    ; Access array elements
    LEA EBX, [BYTE_LIST]
    MOV AL, [EBX]           ; First byte
    MOV AL, [EBX+2]         ; Third byte

    LEA EBX, [DWORD_LIST]
    MOV EAX, [EBX]          ; First dword
    MOV EAX, [EBX+4]        ; Second dword
    MOV EAX, [EBX+8]        ; Third dword

    HLT
