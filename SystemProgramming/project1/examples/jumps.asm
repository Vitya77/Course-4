; jumps.asm - Control flow: JMP, Jcc, CALL, RET, LOOP
; Demonstrates conditional/unconditional jumps and subroutines

section .text

main:
    ; Unconditional jump
    JMP skip_this
    MOV EAX, 0xDEAD         ; This is skipped
skip_this:
    MOV EAX, 1              ; Execution continues here

    ; Compare and conditional jump (signed)
    MOV ECX, 10
    CMP ECX, 10
    JE  values_equal         ; Jump if equal (ZF=1)
    JMP not_equal
values_equal:
    MOV EBX, 1              ; They are equal
    JMP done_cmp
not_equal:
    MOV EBX, 0
done_cmp:

    ; Signed comparison
    MOV EAX, -5
    CMP EAX, 0
    JL  is_negative          ; Jump if less (signed)
    JMP not_negative
is_negative:
    NEG EAX                  ; Make it positive
not_negative:

    ; Unsigned comparison
    MOV EAX, 200
    CMP EAX, 100
    JA  is_above             ; Jump if above (unsigned)
    JMP not_above
is_above:
    MOV EDX, 1
not_above:

    ; Loop using LOOP instruction
    MOV ECX, 5              ; Loop counter
    XOR EAX, EAX            ; Accumulator = 0
loop_start:
    ADD EAX, ECX             ; EAX += ECX
    LOOP loop_start          ; Decrement ECX, jump if not zero
    ; EAX = 5+4+3+2+1 = 15

    ; Loop using conditional jump
    MOV ECX, 10
    XOR EBX, EBX
count_loop:
    INC EBX
    DEC ECX
    JNZ count_loop           ; Jump while ECX != 0

    ; Call subroutine
    MOV EAX, 7
    MOV EBX, 3
    CALL add_numbers
    ; EAX now contains 10

    ; Test other conditions
    MOV EAX, 0
    TEST EAX, EAX
    JZ is_zero               ; Jump if zero
    JMP not_zero
is_zero:
    MOV ECX, 1
not_zero:

    ; Overflow test
    MOV EAX, 0x7FFFFFFF
    ADD EAX, 1
    JO overflow_occurred     ; Jump on overflow
    JMP no_overflow
overflow_occurred:
    MOV EDX, 0xFF
no_overflow:

    HLT

; Subroutine: add EAX + EBX, result in EAX
add_numbers:
    ADD EAX, EBX
    RET
