; arithmetic.asm - Multiplication, division, shifts, rotates
; Demonstrates: MUL, IMUL, DIV, IDIV, SHL, SHR, SAR, ROL, ROR

section .text

start:
    ; --- Unsigned multiplication (MUL) ---
    ; MUL r/m32: EDX:EAX = EAX * operand
    MOV EAX, 100
    MOV EBX, 50
    MUL EBX                  ; EDX:EAX = 100 * 50 = 5000

    ; 8-bit multiply: AX = AL * r/m8
    MOV AL, 25
    MOV BL, 10
    MUL BL                   ; AX = 25 * 10 = 250

    ; --- Signed multiplication (IMUL) ---
    ; One-operand form: EDX:EAX = EAX * operand
    MOV EAX, -10
    MOV ECX, 7
    IMUL ECX                 ; EDX:EAX = -10 * 7 = -70

    ; Two-operand form: dst = dst * src
    MOV EAX, 15
    IMUL EAX, EBX            ; EAX = EAX * EBX

    ; Three-operand form: dst = src * imm
    IMUL ECX, EBX, 12        ; ECX = EBX * 12

    ; --- Unsigned division (DIV) ---
    ; DIV r/m32: EAX = EDX:EAX / operand, EDX = remainder
    XOR EDX, EDX             ; Clear upper 32 bits
    MOV EAX, 100
    MOV ECX, 7
    DIV ECX                  ; EAX = 100/7 = 14, EDX = 100%7 = 2

    ; --- Signed division (IDIV) ---
    MOV EAX, -100
    CDQ                      ; Sign-extend EAX into EDX:EAX
    MOV ECX, 7
    IDIV ECX                 ; EAX = -100/7 = -14, EDX = -100%7 = -2

    ; --- Shift left (SHL) ---
    ; Equivalent to multiplication by powers of 2
    MOV EAX, 1
    SHL EAX, 1               ; EAX = 2  (1 << 1)
    SHL EAX, 3               ; EAX = 16 (2 << 3)
    MOV CL, 4
    SHL EAX, CL              ; EAX = 256 (16 << 4)

    ; --- Shift right logical (SHR) ---
    ; Unsigned division by powers of 2
    MOV EAX, 256
    SHR EAX, 1               ; EAX = 128
    SHR EAX, 3               ; EAX = 16

    ; --- Shift right arithmetic (SAR) ---
    ; Preserves sign bit (for signed division)
    MOV EAX, -128
    SAR EAX, 1               ; EAX = -64 (sign preserved)
    SAR EAX, 2               ; EAX = -16

    ; --- Rotate left (ROL) ---
    MOV EAX, 0x80000001
    ROL EAX, 1               ; Rotate: MSB goes to LSB + CF
    ROL EAX, 4               ; Rotate left by 4

    ; --- Rotate right (ROR) ---
    MOV EAX, 0x80000001
    ROR EAX, 1               ; Rotate: LSB goes to MSB + CF
    ROR EAX, 4               ; Rotate right by 4

    ; --- ADC and SBB (multi-precision arithmetic) ---
    ; 64-bit addition using 32-bit registers
    ; Add 0x00000001_FFFFFFFF + 0x00000000_00000001
    MOV EAX, 0xFFFFFFFF      ; Low 32 bits of first number
    MOV EDX, 1               ; High 32 bits of first number
    ADD EAX, 1               ; Add low parts (sets CF)
    ADC EDX, 0               ; Add high parts + carry

    ; 64-bit subtraction
    MOV EAX, 0x00000000      ; Low 32 bits
    MOV EDX, 2               ; High 32 bits
    SUB EAX, 1               ; Subtract low (borrows)
    SBB EDX, 0               ; Subtract high + borrow

    HLT
