; complex.asm - Full program combining all assembler features
; A complete Linux-like program demonstrating all capabilities:
; - All addressing modes
; - Data directives (DB, DW, DD, EQU, TIMES)
; - Arithmetic, logical, shift, and string operations
; - Subroutines with proper stack frames
; - Conditional jumps and loops
; - System call interface (INT 0x80)

; =========================================================================
; Constants (EQU)
; =========================================================================
SYS_EXIT:     EQU 1
SYS_WRITE:    EQU 4
STDOUT:       EQU 1
NEWLINE_CH:   EQU 10
ARRAY_SIZE:   EQU 8

; =========================================================================
; Data Section
; =========================================================================
section .data

; Strings
HELLO_MSG:    DB 'x86-32 Assembler Test Program', 10, 0
HELLO_LEN:    EQU 30
RESULT_MSG:   DB 'Computation complete.', 10, 0
RESULT_LEN:   EQU 22

; Numeric data
NUMBERS:      DD 64, 34, 25, 12, 22, 11, 90, 1  ; Array to sort
NUM_COUNT:    DD ARRAY_SIZE
SORTED:       TIMES 8 DD 0                       ; Output buffer

; Lookup table (powers of 2)
POW2_TABLE:   DD 1, 2, 4, 8, 16, 32, 64, 128, 256, 512

; Byte patterns
PATTERN_A:    DB 0AAh, 055h, 0AAh, 055h
PATTERN_B:    DW 1234h, 5678h

; BSS-like (initialized to zero)
TEMP_BUF:     TIMES 32 DB 0
TEMP_DWORD:   DD 0

; =========================================================================
; Code Section
; =========================================================================
section .text

; -------------------------------------------------------------------------
; Program entry point
; -------------------------------------------------------------------------
_start:
    ; --- Write hello message using INT 0x80 (Linux syscall) ---
    MOV EAX, SYS_WRITE      ; syscall: write
    MOV EBX, STDOUT          ; fd: stdout
    LEA ECX, [HELLO_MSG]     ; buffer pointer
    MOV EDX, HELLO_LEN       ; length
    INT 0x80                 ; invoke kernel

    ; --- Demonstrate all addressing modes ---
    CALL demo_addressing

    ; --- Bubble sort the NUMBERS array ---
    LEA ESI, [NUMBERS]       ; Source array
    MOV ECX, ARRAY_SIZE      ; Element count
    CALL bubble_sort

    ; --- Find minimum and maximum ---
    LEA ESI, [NUMBERS]
    MOV ECX, ARRAY_SIZE
    CALL find_min_max
    ; EAX = min, EBX = max

    ; --- Compute sum using LOOP ---
    LEA ESI, [NUMBERS]
    MOV ECX, ARRAY_SIZE
    CALL array_sum
    ; EAX = sum of all elements

    ; --- Demonstrate bit manipulation ---
    CALL demo_bitops

    ; --- Write completion message ---
    MOV EAX, SYS_WRITE
    MOV EBX, STDOUT
    LEA ECX, [RESULT_MSG]
    MOV EDX, RESULT_LEN
    INT 0x80

    ; --- Exit program ---
    MOV EAX, SYS_EXIT
    XOR EBX, EBX            ; exit code 0
    INT 0x80

; -------------------------------------------------------------------------
; demo_addressing: Exercise all 6 addressing modes
; -------------------------------------------------------------------------
demo_addressing:
    PUSH EBP
    MOV EBP, ESP
    PUSHAD

    ; Mode 1: Register
    MOV EAX, ECX
    XOR EDX, EDX

    ; Mode 2: Immediate
    MOV EAX, 0xDEADBEEF
    ADD EBX, 42

    ; Mode 3: Direct memory
    MOV EAX, [NUM_COUNT]
    MOV DWORD [TEMP_DWORD], 999

    ; Mode 4: Register indirect
    LEA ESI, [PATTERN_A]
    MOV AL, [ESI]
    MOV BYTE [ESI], 0xFF

    ; Mode 5: Base + displacement
    LEA EBX, [NUMBERS]
    MOV EAX, [EBX+4]        ; Second element
    MOV ECX, [EBX+12]       ; Fourth element
    MOV EAX, [EBP-4]        ; Local variable area

    ; Mode 6: SIB (Base + Index*Scale + Disp)
    LEA EBX, [NUMBERS]
    MOV ECX, 3               ; index
    MOV EAX, [EBX+ECX*4]    ; NUMBERS[3]
    MOV EDX, [EBX+ECX*4+4]  ; NUMBERS[4]

    ; Lookup table with scale
    LEA EBX, [POW2_TABLE]
    MOV ECX, 5
    MOV EAX, [EBX+ECX*4]    ; POW2_TABLE[5] = 32

    POPAD
    POP EBP
    RET

; -------------------------------------------------------------------------
; bubble_sort: Sort array in-place (ascending)
; Input: ESI = array pointer, ECX = count
; -------------------------------------------------------------------------
bubble_sort:
    PUSH EBP
    MOV EBP, ESP
    PUSH EBX
    PUSH ECX
    PUSH EDX
    PUSH ESI
    PUSH EDI

    MOV EDI, ECX            ; EDI = n (total elements)
    DEC EDI                  ; EDI = n-1 (outer loop count)

outer_loop:
    CMP EDI, 0
    JLE sort_done
    XOR EBX, EBX            ; swapped = false
    XOR ECX, ECX            ; j = 0

inner_loop:
    CMP ECX, EDI
    JGE outer_next

    ; Compare adjacent elements: arr[j] vs arr[j+1]
    MOV EAX, [ESI+ECX*4]    ; arr[j]
    MOV EDX, [ESI+ECX*4+4]  ; arr[j+1]
    CMP EAX, EDX
    JLE no_swap

    ; Swap them
    MOV [ESI+ECX*4], EDX
    MOV [ESI+ECX*4+4], EAX
    MOV EBX, 1              ; swapped = true

no_swap:
    INC ECX
    JMP inner_loop

outer_next:
    CMP EBX, 0
    JE sort_done             ; No swaps -> already sorted
    DEC EDI
    JMP outer_loop

sort_done:
    POP EDI
    POP ESI
    POP EDX
    POP ECX
    POP EBX
    POP EBP
    RET

; -------------------------------------------------------------------------
; find_min_max: Find minimum and maximum in array
; Input: ESI = array, ECX = count
; Output: EAX = min, EBX = max
; -------------------------------------------------------------------------
find_min_max:
    PUSH EBP
    MOV EBP, ESP
    PUSH ECX
    PUSH EDX

    MOV EAX, [ESI]          ; min = arr[0]
    MOV EBX, [ESI]          ; max = arr[0]
    MOV EDX, 1              ; i = 1

mm_loop:
    CMP EDX, ECX
    JGE mm_done

    MOV EDI, [ESI+EDX*4]    ; arr[i]

    ; Check min
    CMP EDI, EAX
    JGE not_new_min
    MOV EAX, EDI
not_new_min:

    ; Check max
    CMP EDI, EBX
    JLE not_new_max
    MOV EBX, EDI
not_new_max:

    INC EDX
    JMP mm_loop

mm_done:
    POP EDX
    POP ECX
    POP EBP
    RET

; -------------------------------------------------------------------------
; array_sum: Sum all elements using LOOP
; Input: ESI = array, ECX = count
; Output: EAX = sum
; -------------------------------------------------------------------------
array_sum:
    PUSH EBP
    MOV EBP, ESP
    PUSH EBX

    XOR EAX, EAX            ; sum = 0
    XOR EBX, EBX            ; index = 0

sum_loop:
    ADD EAX, [ESI+EBX*4]    ; sum += arr[index]
    INC EBX
    LOOP sum_loop            ; dec ECX, jump if not zero

    POP EBX
    POP EBP
    RET

; -------------------------------------------------------------------------
; demo_bitops: Demonstrate bit manipulation
; -------------------------------------------------------------------------
demo_bitops:
    PUSH EBP
    MOV EBP, ESP
    PUSH EBX
    PUSH ECX

    ; --- Shifts ---
    MOV EAX, 1
    SHL EAX, 8              ; EAX = 256
    SHR EAX, 4              ; EAX = 16
    MOV EBX, 0x80000000
    SAR EBX, 16             ; Sign-extended shift right

    ; --- Rotates ---
    MOV EAX, 0xDEADBEEF
    ROL EAX, 8              ; Rotate left 8 bits
    ROR EAX, 4              ; Rotate right 4 bits

    ; --- Bit testing ---
    MOV EAX, 0xFF00FF00
    TEST EAX, 0x00FF0000    ; Test specific bits
    JZ bits_clear
    MOV ECX, 1              ; Some bits were set
    JMP bits_done
bits_clear:
    MOV ECX, 0
bits_done:

    ; --- XOR swap trick ---
    MOV EAX, 0xAAAAAAAA
    MOV EBX, 0x55555555
    XOR EAX, EBX
    XOR EBX, EAX
    XOR EAX, EBX
    ; Now EAX=0x55555555, EBX=0xAAAAAAAA (swapped!)

    ; --- Masking ---
    MOV EAX, 0xDEADBEEF
    AND EAX, 0x0000FFFF     ; Keep low 16 bits
    OR  EAX, 0xCAFE0000     ; Set high 16 bits

    POP ECX
    POP EBX
    POP EBP
    RET
