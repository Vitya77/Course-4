; addressing.asm - All 6 x86 addressing modes
; Demonstrates every fundamental addressing mode

section .data

MY_VAR:   DD 12345678h       ; A 32-bit variable
ARRAY:    DD 10, 20, 30, 40  ; Array of dwords
BUFFER:   TIMES 16 DB 0      ; 16-byte buffer

section .text

; Mode 1: Register addressing
; Operands are registers only
    MOV EAX, EBX             ; reg <- reg
    ADD ECX, EDX             ; reg <- reg
    XOR ESI, EDI             ; reg <- reg

; Mode 2: Immediate addressing
; Source is a constant value
    MOV EAX, 42              ; reg <- imm (decimal)
    MOV EBX, 0xFF            ; reg <- imm (hex)
    ADD ECX, 100             ; reg <- imm
    CMP EDX, 0               ; compare with immediate

; Mode 3: Direct memory addressing
; Address is a label (absolute address)
    MOV EAX, [MY_VAR]        ; Load from memory label
    MOV [MY_VAR], EBX        ; Store to memory label
    ADD DWORD [MY_VAR], 1    ; Increment memory directly

; Mode 4: Register indirect addressing
; Address is in a register
    LEA EBX, [ARRAY]         ; Load address of ARRAY
    MOV EAX, [EBX]           ; Load via register
    MOV [EBX], ECX           ; Store via register
    LEA ESI, [BUFFER]
    MOV BYTE [ESI], 0x41     ; Store byte via register

; Mode 5: Base + displacement addressing
; Register + constant offset
    MOV EAX, [EBX+4]         ; Second element of array
    MOV ECX, [EBX+8]         ; Third element
    MOV EDX, [EBX+12]        ; Fourth element
    MOV EAX, [EBP+8]         ; Typical stack frame access
    MOV EBX, [EBP-4]         ; Local variable access

; Mode 6: Base + Index*Scale + Displacement (SIB)
; Full SIB addressing: base + index*scale + displacement
    MOV ECX, 2               ; Index = 2
    MOV EAX, [EBX+ECX*4]     ; ARRAY[ECX] - scaled index
    MOV EAX, [EBX+ECX*4+4]   ; ARRAY[ECX+1] with displacement
    MOV EDX, [ESI+EDI*1]     ; Base + index, scale=1
    MOV EAX, [EBX+ECX*8+16]  ; Full SIB with scale=8

    HLT
