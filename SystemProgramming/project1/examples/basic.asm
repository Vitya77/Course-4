; basic.asm - Basic MOV, arithmetic, and logical operations
; Demonstrates: MOV, ADD, SUB, AND, OR, XOR, CMP, INC, DEC

section .text

start:
    ; Data movement between registers
    MOV EAX, 100        ; Load immediate value
    MOV EBX, 200        ; Load another value
    MOV ECX, EAX        ; Register to register
    MOV DL, 0FFh        ; 8-bit immediate (hex)

    ; Arithmetic operations
    ADD EAX, EBX        ; EAX = EAX + EBX = 300
    SUB ECX, 50         ; ECX = 100 - 50 = 50
    ADD EAX, 10         ; Add immediate
    SUB EBX, 20         ; Subtract immediate

    ; Increment and decrement
    INC EAX             ; EAX++
    DEC EBX             ; EBX--
    INC ECX             ; ECX++
    DEC DL              ; DL--

    ; Logical operations
    MOV EAX, 0xFF00FF00
    MOV EBX, 0x0F0F0F0F
    AND EAX, EBX        ; Bitwise AND
    OR  EAX, 0x80000000 ; Set MSB
    XOR ECX, ECX        ; Clear register (common idiom)
    NOT EBX             ; Bitwise NOT

    ; Comparison
    CMP EAX, EBX        ; Compare (sets flags)
    CMP ECX, 0          ; Compare with zero

    ; Negate
    MOV EAX, 42
    NEG EAX             ; EAX = -42 (two's complement)

    HLT
