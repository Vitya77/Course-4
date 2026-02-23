; stack.asm - Stack operations and calling conventions
; Demonstrates: PUSH, POP, stack frames, CALL, RET, PUSHAD, POPAD

section .data

RESULT:   DD 0

section .text

main:
    ; --- Basic PUSH/POP ---
    PUSH EAX                 ; Save EAX on stack
    PUSH EBX                 ; Save EBX on stack
    PUSH 42                  ; Push immediate value

    POP ECX                  ; ECX = 42
    POP EBX                  ; Restore EBX
    POP EAX                  ; Restore EAX

    ; --- PUSHAD/POPAD - save/restore all general-purpose registers ---
    PUSHAD                   ; Push EAX,ECX,EDX,EBX,ESP,EBP,ESI,EDI
    ; ... do work ...
    MOV EAX, 0xFF
    MOV EBX, 0xAA
    POPAD                    ; Restore all registers

    ; --- Function call with cdecl convention ---
    ; Calculate: add(10, 20)
    PUSH 20                  ; Push second argument
    PUSH 10                  ; Push first argument
    CALL func_add            ; Call function
    ADD ESP, 8               ; Clean up arguments (caller cleanup)
    MOV [RESULT], EAX        ; Store result (30)

    ; --- Function call with multiple args ---
    ; Calculate: multiply(6, 7)
    PUSH 7
    PUSH 6
    CALL func_multiply
    ADD ESP, 8
    ; EAX = 42

    ; --- Nested calls ---
    PUSH 3
    PUSH 4
    CALL func_add
    ADD ESP, 8
    PUSH EAX                 ; Push result of add(4,3) = 7
    PUSH 6
    CALL func_multiply
    ADD ESP, 8
    ; EAX = 6 * 7 = 42

    HLT

; Function: add(a, b) -> EAX
; cdecl calling convention with stack frame
func_add:
    PUSH EBP                 ; Save old base pointer
    MOV EBP, ESP             ; Set up stack frame
    MOV EAX, [EBP+8]        ; First argument (a)
    ADD EAX, [EBP+12]       ; Add second argument (b)
    MOV ESP, EBP             ; Tear down stack frame
    POP EBP                  ; Restore base pointer
    RET                      ; Return (EAX = a + b)

; Function: multiply(a, b) -> EAX
; Uses LEAVE instruction for epilogue
func_multiply:
    PUSH EBP
    MOV EBP, ESP
    PUSH EBX                 ; Save callee-saved register

    MOV EAX, [EBP+8]        ; a
    MOV EBX, [EBP+12]       ; b
    IMUL EAX, EBX            ; EAX = a * b

    POP EBX                  ; Restore EBX
    LEAVE                    ; Equivalent to: MOV ESP,EBP / POP EBP
    RET

; Function: factorial(n) -> EAX (recursive)
; Demonstrates recursive calls and stack frame usage
func_factorial:
    PUSH EBP
    MOV EBP, ESP

    MOV EAX, [EBP+8]        ; n
    CMP EAX, 1
    JLE base_case            ; if n <= 1, return 1

    DEC EAX                  ; n - 1
    PUSH EAX                 ; push (n-1) as argument
    CALL func_factorial      ; recursive call
    ADD ESP, 4               ; clean up argument

    ; Multiply result by n
    MOV EBX, [EBP+8]        ; n (original)
    IMUL EAX, EBX            ; EAX = factorial(n-1) * n

    POP EBP
    RET

base_case:
    MOV EAX, 1              ; return 1
    POP EBP
    RET
