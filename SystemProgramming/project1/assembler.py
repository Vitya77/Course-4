"""
x86-32 Two-Pass Assembler Engine
Supports NASM-like syntax with 50+ mnemonics, all 6 addressing modes,
ModR/M + SIB byte encoding, and formatted listing generation.
"""

from dataclasses import dataclass, field
from typing import List, Optional, Dict, Tuple
import re
import struct


def _pack_i32(val: int) -> bytes:
    """Pack a 32-bit value, handling unsigned values > 0x7FFFFFFF."""
    return struct.pack('<I', val & 0xFFFFFFFF)


def _pack_disp32(val: int) -> bytes:
    """Pack a 32-bit signed displacement."""
    return struct.pack('<I', val & 0xFFFFFFFF)


# ---------------------------------------------------------------------------
# Register tables
# ---------------------------------------------------------------------------

REGS_32 = {
    'EAX': 0, 'ECX': 1, 'EDX': 2, 'EBX': 3,
    'ESP': 4, 'EBP': 5, 'ESI': 6, 'EDI': 7,
}
REGS_16 = {
    'AX': 0, 'CX': 1, 'DX': 2, 'BX': 3,
    'SP': 4, 'BP': 5, 'SI': 6, 'DI': 7,
}
REGS_8 = {
    'AL': 0, 'CL': 1, 'DL': 2, 'BL': 3,
    'AH': 4, 'CH': 5, 'DH': 6, 'BH': 7,
}
SEGMENT_REGS = {
    'CS': 1, 'DS': 3, 'ES': 0, 'FS': 4, 'GS': 5, 'SS': 2,
}

ALL_REGS = {}
ALL_REGS.update(REGS_32)
ALL_REGS.update(REGS_16)
ALL_REGS.update(REGS_8)
ALL_REGS.update(SEGMENT_REGS)


def reg_size(name: str) -> int:
    name = name.upper()
    if name in REGS_32:
        return 32
    if name in REGS_16:
        return 16
    if name in REGS_8:
        return 8
    if name in SEGMENT_REGS:
        return 16
    return 0


def reg_code(name: str) -> int:
    return ALL_REGS.get(name.upper(), -1)


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class Operand:
    type: str = ''          # 'reg', 'imm', 'mem', 'label'
    reg: str = ''           # register name
    imm_val: int = 0        # immediate value
    mem_base: str = ''      # base register
    mem_index: str = ''     # index register
    mem_scale: int = 1      # scale factor (1,2,4,8)
    mem_disp: int = 0       # displacement
    mem_disp_label: str = ''  # label used as displacement/address
    size_hint: int = 0      # explicit size override: 8, 16, 32
    raw: str = ''           # original text


@dataclass
class ParsedLine:
    line_num: int = 0
    raw_text: str = ''
    label: str = ''
    mnemonic: str = ''
    operands: List[Operand] = field(default_factory=list)
    directive: str = ''
    directive_args: str = ''
    address: int = 0
    size: int = 0
    machine_code: bytes = b''
    error: str = ''
    is_equ: bool = False
    equ_value: int = 0
    times_count: int = 0
    times_inner: str = ''
    prefix: str = ''


@dataclass
class AssemblyResult:
    success: bool = False
    machine_code: bytes = b''
    listing: str = ''
    errors: List[str] = field(default_factory=list)
    symbols: Dict[str, int] = field(default_factory=dict)


# ---------------------------------------------------------------------------
# Condition code table for Jcc instructions
# ---------------------------------------------------------------------------

JCC_CODES = {
    'JO': 0x0, 'JNO': 0x1, 'JB': 0x2, 'JC': 0x2, 'JNAE': 0x2,
    'JNB': 0x3, 'JNC': 0x3, 'JAE': 0x3,
    'JE': 0x4, 'JZ': 0x4, 'JNE': 0x5, 'JNZ': 0x5,
    'JBE': 0x6, 'JNA': 0x6, 'JNBE': 0x7, 'JA': 0x7,
    'JS': 0x8, 'JNS': 0x9,
    'JP': 0xA, 'JPE': 0xA, 'JNP': 0xB, 'JPO': 0xB,
    'JL': 0xC, 'JNGE': 0xC, 'JNL': 0xD, 'JGE': 0xD,
    'JLE': 0xE, 'JNG': 0xE, 'JNLE': 0xF, 'JG': 0xF,
}

# Simple (no-operand) instructions
SIMPLE_OPCODES = {
    'NOP': b'\x90', 'HLT': b'\xF4', 'RET': b'\xC3',
    'CLI': b'\xFA', 'STI': b'\xFB',
    'CLC': b'\xF8', 'STC': b'\xF9', 'CMC': b'\xF5',
    'CLD': b'\xFC', 'STD': b'\xFD',
    'PUSHAD': b'\x60', 'POPAD': b'\x61',
    'PUSHFD': b'\x9C', 'POPFD': b'\x9D',
    'LEAVE': b'\xC9',
    'MOVSB': b'\xA4', 'MOVSD': b'\xA5',
    'STOSB': b'\xAA', 'STOSD': b'\xAB',
    'LODSB': b'\xAC', 'LODSD': b'\xAD',
    'CMPSB': b'\xA6', 'CMPSD': b'\xA7',
    'SCASB': b'\xAE', 'SCASD': b'\xAF',
    'CBW': b'\x66\x98', 'CWDE': b'\x98',
    'CDQ': b'\x99', 'CWD': b'\x66\x99',
    'SAHF': b'\x9E', 'LAHF': b'\x9F',
    'INT3': b'\xCC',
    'IRET': b'\xCF',
    'XLAT': b'\xD7',
    'AAA': b'\x37', 'AAS': b'\x3F', 'DAA': b'\x27', 'DAS': b'\x2F',
}

# ALU opcodes sharing /r encoding pattern:  opcode_base + reg encoding
# Format: mnemonic -> (base_opcode_byte, /digit for r/m forms)
ALU_OPS = {
    'ADD': (0x00, 0), 'OR':  (0x08, 1), 'ADC': (0x10, 2), 'SBB': (0x18, 3),
    'AND': (0x20, 4), 'SUB': (0x28, 5), 'XOR': (0x30, 6), 'CMP': (0x38, 7),
}

SHIFT_OPS = {
    'ROL': 0, 'ROR': 1, 'RCL': 2, 'RCR': 3,
    'SHL': 4, 'SAL': 4, 'SHR': 5, 'SAR': 7,
}

PREFIX_BYTES = {
    'REP': 0xF3, 'REPE': 0xF3, 'REPZ': 0xF3,
    'REPNE': 0xF2, 'REPNZ': 0xF2,
    'LOCK': 0xF0,
}


# ---------------------------------------------------------------------------
# Assembler class
# ---------------------------------------------------------------------------

class Assembler:
    def __init__(self):
        self.lines: List[ParsedLine] = []
        self.symbols: Dict[str, int] = {}
        self.origin: int = 0
        self.current_address: int = 0
        self.errors: List[str] = []
        self.sections: Dict[str, int] = {}

    # -----------------------------------------------------------------------
    # Public API
    # -----------------------------------------------------------------------

    def assemble(self, source: str) -> AssemblyResult:
        self.lines = []
        self.symbols = {}
        self.origin = 0
        self.current_address = 0
        self.errors = []

        raw_lines = source.replace('\r\n', '\n').split('\n')

        # Parse all lines
        for i, raw in enumerate(raw_lines, 1):
            parsed = self._parse_line(raw, i)
            self.lines.append(parsed)

        # Expand TIMES directives
        self._expand_times()

        # Pass 1: size calculation + symbol table
        self._pass1()

        # Pass 2: code generation
        self._pass2()

        # Build result
        all_code = b''
        for ln in self.lines:
            if ln.machine_code:
                all_code += ln.machine_code

        listing = self._generate_listing()

        return AssemblyResult(
            success=len(self.errors) == 0,
            machine_code=all_code,
            listing=listing,
            errors=list(self.errors),
            symbols=dict(self.symbols),
        )

    # -----------------------------------------------------------------------
    # Parsing
    # -----------------------------------------------------------------------

    def _parse_line(self, raw: str, line_num: int) -> ParsedLine:
        pl = ParsedLine(line_num=line_num, raw_text=raw)

        text = raw.strip()

        # Remove comment
        in_string = False
        str_char = None
        comment_pos = -1
        for i, ch in enumerate(text):
            if in_string:
                if ch == str_char:
                    in_string = False
            else:
                if ch in ('"', "'"):
                    in_string = True
                    str_char = ch
                elif ch == ';':
                    comment_pos = i
                    break
        if comment_pos >= 0:
            text = text[:comment_pos].strip()

        if not text:
            return pl

        # Check for label
        m = re.match(r'^([A-Za-z_]\w*)\s*:', text)
        if m:
            pl.label = m.group(1).upper()
            text = text[m.end():].strip()

        if not text:
            return pl

        # Check for prefix
        first_word = text.split()[0].upper()
        if first_word in PREFIX_BYTES:
            pl.prefix = first_word
            text = text[len(first_word):].strip()
            if not text:
                return pl

        # Get mnemonic
        parts = text.split(None, 1)
        mnemonic = parts[0].upper()
        rest = parts[1].strip() if len(parts) > 1 else ''

        # Check for TIMES directive
        if mnemonic == 'TIMES':
            m2 = re.match(r'(\d+)\s+(.*)', rest)
            if m2:
                pl.times_count = int(m2.group(1))
                pl.times_inner = m2.group(2).strip()
                pl.directive = 'TIMES'
                return pl

        # Handle ORG
        if mnemonic == 'ORG':
            pl.directive = 'ORG'
            pl.directive_args = rest
            return pl

        # Handle SECTION / SEGMENT
        if mnemonic in ('SECTION', 'SEGMENT'):
            pl.directive = 'SECTION'
            pl.directive_args = rest
            return pl

        # Handle data directives
        if mnemonic in ('DB', 'DW', 'DD', 'DQ', 'RESB', 'RESW', 'RESD'):
            pl.directive = mnemonic
            pl.directive_args = rest
            return pl

        # Handle EQU
        if mnemonic == 'EQU':
            pl.is_equ = True
            pl.directive = 'EQU'
            pl.directive_args = rest
            return pl

        # Handle GLOBAL/EXTERN
        if mnemonic in ('GLOBAL', 'EXTERN', 'BITS'):
            pl.directive = mnemonic
            pl.directive_args = rest
            return pl

        pl.mnemonic = mnemonic

        # Parse operands
        if rest:
            pl.operands = self._parse_operands(rest)

        return pl

    def _parse_operands(self, text: str) -> List[Operand]:
        operands = []
        parts = self._split_operands(text)
        for part in parts:
            op = self._parse_single_operand(part.strip())
            operands.append(op)
        return operands

    def _split_operands(self, text: str) -> List[str]:
        result = []
        depth = 0
        current = ''
        in_string = False
        str_char = None

        for ch in text:
            if in_string:
                current += ch
                if ch == str_char:
                    in_string = False
            else:
                if ch in ('"', "'"):
                    in_string = True
                    str_char = ch
                    current += ch
                elif ch == '[':
                    depth += 1
                    current += ch
                elif ch == ']':
                    depth -= 1
                    current += ch
                elif ch == ',' and depth == 0:
                    result.append(current)
                    current = ''
                else:
                    current += ch
        if current.strip():
            result.append(current)
        return result

    def _parse_single_operand(self, text: str) -> Operand:
        op = Operand(raw=text)
        text = text.strip()

        # Check for size override prefix
        size_hint = 0
        for prefix, size in [('BYTE', 8), ('WORD', 16), ('DWORD', 32)]:
            if text.upper().startswith(prefix):
                rest = text[len(prefix):].strip()
                if rest.upper().startswith('PTR'):
                    rest = rest[3:].strip()
                # Also handle just "BYTE [...]"
                if rest.startswith('[') or rest[0:1].isalpha() or rest[0:1].isdigit():
                    text = rest
                    size_hint = size
                    break

        op.size_hint = size_hint

        # Register operand
        if text.upper() in ALL_REGS:
            op.type = 'reg'
            op.reg = text.upper()
            if not size_hint:
                op.size_hint = reg_size(op.reg)
            return op

        # Memory operand: [...]
        if text.startswith('[') and text.endswith(']'):
            inner = text[1:-1].strip()
            op.type = 'mem'
            self._parse_memory_expression(inner, op)
            if not op.size_hint:
                op.size_hint = 32  # default
            return op

        # Immediate: number or label
        op.type = 'imm'
        val = self._try_parse_number(text)
        if val is not None:
            op.imm_val = val
        else:
            # It's a label reference
            op.type = 'label'
            op.mem_disp_label = text.upper()
        return op

    def _parse_memory_expression(self, expr: str, op: Operand):
        expr = expr.strip()
        tokens = self._tokenize_mem_expr(expr)

        sign = 1
        i = 0
        while i < len(tokens):
            tok = tokens[i]

            if tok == '+':
                sign = 1
                i += 1
                continue
            elif tok == '-':
                sign = -1
                i += 1
                continue
            elif tok == '*':
                i += 1
                continue

            tok_upper = tok.upper()

            # Check if it's a register
            if tok_upper in ALL_REGS:
                # Check for scale: REG*N or N*REG
                if i + 2 < len(tokens) and tokens[i + 1] == '*':
                    scale_val = self._try_parse_number(tokens[i + 2])
                    if scale_val is not None and scale_val in (1, 2, 4, 8):
                        op.mem_index = tok_upper
                        op.mem_scale = scale_val
                        i += 3
                        continue

                # Check if previous was a number followed by *
                # This is handled by the number case below

                if not op.mem_base:
                    op.mem_base = tok_upper
                elif not op.mem_index:
                    op.mem_index = tok_upper
                    op.mem_scale = 1
                else:
                    # Third register - error, but try to handle
                    pass
                i += 1
                continue

            # Number
            num = self._try_parse_number(tok)
            if num is not None:
                # Check for N*REG
                if i + 2 < len(tokens) and tokens[i + 1] == '*':
                    next_upper = tokens[i + 2].upper()
                    if next_upper in ALL_REGS:
                        op.mem_index = next_upper
                        op.mem_scale = num
                        i += 3
                        continue
                op.mem_disp += sign * num
                i += 1
                continue

            # Label
            op.mem_disp_label = tok_upper
            i += 1

    def _tokenize_mem_expr(self, expr: str) -> List[str]:
        tokens = []
        i = 0
        while i < len(expr):
            ch = expr[i]
            if ch in ' \t':
                i += 1
                continue
            if ch in '+-*':
                tokens.append(ch)
                i += 1
                continue
            # Collect word/number
            start = i
            while i < len(expr) and expr[i] not in ' \t+-*':
                i += 1
            tokens.append(expr[start:i])
        return tokens

    def _try_parse_number(self, text: str) -> Optional[int]:
        text = text.strip()
        if not text:
            return None
        try:
            # Hex: 0x... or ...h
            if text.startswith('0x') or text.startswith('0X'):
                return int(text, 16)
            if text.upper().endswith('H'):
                return int(text[:-1], 16)
            # Binary: 0b... or ...b
            if text.startswith('0b') or text.startswith('0B'):
                return int(text, 2)
            if text.upper().endswith('B') and all(c in '01' for c in text[:-1]) and len(text) > 1:
                return int(text[:-1], 2)
            # Octal: ...o or ...q
            if text.upper().endswith('O') or text.upper().endswith('Q'):
                return int(text[:-1], 8)
            # Decimal
            return int(text)
        except (ValueError, IndexError):
            return None

    # -----------------------------------------------------------------------
    # TIMES expansion
    # -----------------------------------------------------------------------

    def _expand_times(self):
        expanded = []
        for pl in self.lines:
            if pl.directive == 'TIMES' and pl.times_count > 0:
                for j in range(pl.times_count):
                    inner_parsed = self._parse_line(pl.times_inner, pl.line_num)
                    if j == 0:
                        inner_parsed.label = pl.label
                    inner_parsed.raw_text = pl.raw_text if j == 0 else ''
                    expanded.append(inner_parsed)
            else:
                expanded.append(pl)
        self.lines = expanded

    # -----------------------------------------------------------------------
    # Pass 1: Size calculation + symbol table
    # -----------------------------------------------------------------------

    def _pass1(self):
        self.current_address = self.origin

        for pl in self.lines:
            # Set address
            pl.address = self.current_address

            # Register label
            if pl.label:
                if pl.is_equ:
                    val = self._eval_expr(pl.directive_args)
                    pl.equ_value = val
                    self.symbols[pl.label] = val
                else:
                    self.symbols[pl.label] = self.current_address

            # Handle directives
            if pl.directive:
                size = self._directive_size(pl)
                pl.size = size
                self.current_address += size
                continue

            if not pl.mnemonic:
                continue

            # Calculate instruction size
            size = self._estimate_instruction_size(pl)
            pl.size = size
            self.current_address += size

    def _directive_size(self, pl: ParsedLine) -> int:
        d = pl.directive
        if d == 'ORG':
            val = self._eval_expr(pl.directive_args)
            self.origin = val
            self.current_address = val
            pl.address = val
            return 0
        if d == 'SECTION':
            return 0
        if d == 'EQU':
            return 0
        if d in ('GLOBAL', 'EXTERN', 'BITS'):
            return 0
        if d == 'DB':
            return self._data_directive_size(pl.directive_args, 1)
        if d == 'DW':
            return self._data_directive_size(pl.directive_args, 2)
        if d == 'DD':
            return self._data_directive_size(pl.directive_args, 4)
        if d == 'DQ':
            return self._data_directive_size(pl.directive_args, 8)
        if d == 'RESB':
            return self._eval_expr(pl.directive_args)
        if d == 'RESW':
            return self._eval_expr(pl.directive_args) * 2
        if d == 'RESD':
            return self._eval_expr(pl.directive_args) * 4
        return 0

    def _data_directive_size(self, args: str, elem_size: int) -> int:
        items = self._split_data_args(args)
        total = 0
        for item in items:
            item = item.strip()
            if not item:
                continue
            if (item.startswith('"') and item.endswith('"')) or \
               (item.startswith("'") and item.endswith("'")):
                total += len(self._parse_string_literal(item))
                if elem_size > 1:
                    pass  # strings in DB are byte-by-byte
            else:
                total += elem_size
        return total

    def _split_data_args(self, args: str) -> List[str]:
        result = []
        current = ''
        in_string = False
        str_char = None
        for ch in args:
            if in_string:
                current += ch
                if ch == str_char:
                    in_string = False
            else:
                if ch in ('"', "'"):
                    in_string = True
                    str_char = ch
                    current += ch
                elif ch == ',':
                    result.append(current)
                    current = ''
                else:
                    current += ch
        if current.strip():
            result.append(current)
        return result

    def _parse_string_literal(self, s: str) -> bytes:
        # Remove quotes
        inner = s[1:-1]
        result = bytearray()
        i = 0
        while i < len(inner):
            ch = inner[i]
            if ch == '\\' and i + 1 < len(inner):
                nch = inner[i + 1]
                if nch == 'n':
                    result.append(10)
                elif nch == 'r':
                    result.append(13)
                elif nch == 't':
                    result.append(9)
                elif nch == '0':
                    result.append(0)
                elif nch == '\\':
                    result.append(ord('\\'))
                elif nch == '"':
                    result.append(ord('"'))
                elif nch == "'":
                    result.append(ord("'"))
                else:
                    result.append(ord(nch))
                i += 2
            else:
                result.append(ord(ch))
                i += 1
        return bytes(result)

    def _eval_expr(self, expr: str) -> int:
        expr = expr.strip()
        if not expr:
            return 0

        # Replace known symbols
        for sym, val in self.symbols.items():
            pattern = r'\b' + re.escape(sym) + r'\b'
            expr = re.sub(pattern, str(val), expr, flags=re.IGNORECASE)

        # Replace $ with current address
        expr = expr.replace('$', str(self.current_address))

        # Try to evaluate as number first
        val = self._try_parse_number(expr)
        if val is not None:
            return val

        # Try simple arithmetic evaluation (safe subset)
        try:
            # Only allow digits, operators, parens, spaces, and hex prefix
            safe = re.sub(r'0x[0-9a-fA-F]+', lambda m: str(int(m.group(), 16)), expr)
            safe = re.sub(r'[0-9a-fA-F]+[hH]', lambda m: str(int(m.group()[:-1], 16)), safe)
            if re.match(r'^[\d\s\+\-\*\/\(\)]+$', safe):
                return int(eval(safe))  # noqa: S307 - safe subset
        except Exception:
            pass

        return 0

    # -----------------------------------------------------------------------
    # Instruction size estimation (Pass 1)
    # -----------------------------------------------------------------------

    def _estimate_instruction_size(self, pl: ParsedLine) -> int:
        mn = pl.mnemonic
        ops = pl.operands
        nops = len(ops)
        prefix_size = 1 if pl.prefix else 0

        # Simple no-operand instructions
        if mn in SIMPLE_OPCODES:
            return prefix_size + len(SIMPLE_OPCODES[mn])

        # INT n
        if mn == 'INT':
            return prefix_size + 2

        # RET with immediate
        if mn == 'RET' and nops == 1:
            return prefix_size + 3

        # PUSH/POP
        if mn == 'PUSH':
            if nops == 1:
                op = ops[0]
                if op.type == 'reg' and op.reg.upper() in REGS_32:
                    return prefix_size + 1
                if op.type == 'reg' and op.reg.upper() in REGS_16:
                    return prefix_size + 2  # 66h prefix + opcode
                if op.type == 'reg' and op.reg.upper() in SEGMENT_REGS:
                    return prefix_size + 1
                if op.type in ('imm', 'label'):
                    return prefix_size + 5
                if op.type == 'mem':
                    return prefix_size + 2 + self._modrm_extra_size(op)
            return prefix_size + 5  # worst case

        if mn == 'POP':
            if nops == 1:
                op = ops[0]
                if op.type == 'reg' and op.reg.upper() in REGS_32:
                    return prefix_size + 1
                if op.type == 'reg' and op.reg.upper() in REGS_16:
                    return prefix_size + 2
                if op.type == 'reg' and op.reg.upper() in SEGMENT_REGS:
                    return prefix_size + 1
                if op.type == 'mem':
                    return prefix_size + 2 + self._modrm_extra_size(op)
            return prefix_size + 1

        # XCHG
        if mn == 'XCHG':
            if nops == 2 and ops[0].type == 'reg' and ops[1].type == 'reg':
                if ops[0].reg == 'EAX' or ops[1].reg == 'EAX':
                    return prefix_size + 1
                return prefix_size + 2
            return prefix_size + 2 + self._max_modrm_extra(ops)

        # LEA
        if mn == 'LEA':
            return prefix_size + 2 + self._modrm_extra_size(ops[1]) if nops >= 2 else prefix_size + 6

        # MOVZX, MOVSX
        if mn in ('MOVZX', 'MOVSX'):
            return prefix_size + 3 + (self._modrm_extra_size(ops[1]) if nops >= 2 and ops[1].type == 'mem' else 0)

        # ALU ops
        if mn in ALU_OPS:
            return self._estimate_alu_size(pl, prefix_size)

        # TEST
        if mn == 'TEST':
            return self._estimate_test_size(pl, prefix_size)

        # INC/DEC
        if mn in ('INC', 'DEC'):
            if nops == 1 and ops[0].type == 'reg' and ops[0].reg in REGS_32:
                return prefix_size + 1
            return prefix_size + 2 + (self._modrm_extra_size(ops[0]) if nops == 1 and ops[0].type == 'mem' else 0)

        # NEG/NOT
        if mn in ('NEG', 'NOT'):
            return prefix_size + 2 + (self._modrm_extra_size(ops[0]) if nops == 1 and ops[0].type == 'mem' else 0)

        # MUL/IMUL/DIV/IDIV
        if mn in ('MUL', 'DIV', 'IDIV'):
            return prefix_size + 2 + (self._modrm_extra_size(ops[0]) if nops == 1 and ops[0].type == 'mem' else 0)

        if mn == 'IMUL':
            if nops == 1:
                return prefix_size + 2 + (self._modrm_extra_size(ops[0]) if ops[0].type == 'mem' else 0)
            if nops == 2:
                return prefix_size + 3 + (self._modrm_extra_size(ops[1]) if ops[1].type == 'mem' else 0)
            if nops == 3:
                return prefix_size + 3 + (self._modrm_extra_size(ops[1]) if ops[1].type == 'mem' else 0) + 4

        # Shifts/Rotates
        if mn in SHIFT_OPS:
            base = prefix_size + 2
            if nops >= 1 and ops[0].type == 'mem':
                base += self._modrm_extra_size(ops[0])
            if nops >= 2 and ops[1].type == 'imm' and ops[1].imm_val != 1:
                base += 1  # imm8
            return base

        # MOV
        if mn == 'MOV':
            return self._estimate_mov_size(pl, prefix_size)

        # JMP
        if mn == 'JMP':
            if nops == 1:
                op = ops[0]
                if op.type in ('label', 'imm'):
                    return prefix_size + 5  # near jump
                if op.type == 'reg':
                    return prefix_size + 2
                if op.type == 'mem':
                    return prefix_size + 2 + self._modrm_extra_size(op)
            return prefix_size + 5

        # Jcc
        if mn in JCC_CODES:
            return prefix_size + 6  # 0F 8x rel32

        # CALL
        if mn == 'CALL':
            if nops == 1:
                op = ops[0]
                if op.type in ('label', 'imm'):
                    return prefix_size + 5
                if op.type == 'reg':
                    return prefix_size + 2
                if op.type == 'mem':
                    return prefix_size + 2 + self._modrm_extra_size(op)
            return prefix_size + 5

        # LOOP, LOOPE, LOOPNE
        if mn in ('LOOP', 'LOOPE', 'LOOPZ', 'LOOPNE', 'LOOPNZ'):
            return prefix_size + 2  # opcode + rel8

        # JECXZ
        if mn == 'JECXZ':
            return prefix_size + 2

        # Default fallback
        return prefix_size + 6

    def _modrm_extra_size(self, op: Operand) -> int:
        if op.type != 'mem':
            return 0
        extra = 0
        base = op.mem_base
        index = op.mem_index
        has_label = bool(op.mem_disp_label)

        # SIB needed?
        need_sib = bool(index) or (base == 'ESP')
        if need_sib:
            extra += 1

        # Displacement size
        if has_label or (not base and not index):
            extra += 4  # disp32
        elif not base and index:
            extra += 4  # SIB with no base needs disp32
        elif base == 'EBP' and op.mem_disp == 0 and not has_label:
            extra += 1  # [EBP] needs disp8=0
        elif op.mem_disp != 0:
            if -128 <= op.mem_disp <= 127:
                extra += 1
            else:
                extra += 4
        elif base == 'EBP':
            extra += 1  # [EBP] always needs disp8

        return extra

    def _max_modrm_extra(self, ops: List[Operand]) -> int:
        return max((self._modrm_extra_size(op) for op in ops if op.type == 'mem'), default=0)

    def _estimate_alu_size(self, pl: ParsedLine, prefix_size: int) -> int:
        ops = pl.operands
        if len(ops) != 2:
            return prefix_size + 6

        dst, src = ops[0], ops[1]

        # r/m, imm
        if src.type in ('imm', 'label'):
            imm = src.imm_val
            base = prefix_size + 2  # opcode + modrm
            if dst.type == 'mem':
                base += self._modrm_extra_size(dst)
            # Check for AL/AX/EAX short form
            if dst.type == 'reg' and dst.reg in ('AL', 'AX', 'EAX'):
                if dst.reg == 'AL':
                    return prefix_size + 2  # opcode + imm8
                elif dst.reg == 'AX':
                    return prefix_size + 4  # 66h + opcode + imm16
                else:
                    # EAX: check if imm fits in imm8
                    if src.type == 'imm' and -128 <= imm <= 127:
                        return prefix_size + 3  # 83 /d imm8
                    return prefix_size + 5  # opcode + imm32
            # r/m32, imm8
            if src.type == 'imm' and -128 <= imm <= 127:
                return base + 1  # imm8
            return base + 4  # imm32

        # r/m, r  or  r, r/m
        base = prefix_size + 2  # opcode + modrm
        if dst.type == 'mem':
            base += self._modrm_extra_size(dst)
        elif src.type == 'mem':
            base += self._modrm_extra_size(src)
        return base

    def _estimate_test_size(self, pl: ParsedLine, prefix_size: int) -> int:
        ops = pl.operands
        if len(ops) != 2:
            return prefix_size + 6
        dst, src = ops[0], ops[1]
        if src.type in ('imm', 'label'):
            if dst.type == 'reg' and dst.reg == 'AL':
                return prefix_size + 2
            if dst.type == 'reg' and dst.reg == 'EAX':
                return prefix_size + 5
            base = prefix_size + 2
            if dst.type == 'mem':
                base += self._modrm_extra_size(dst)
            return base + 4
        base = prefix_size + 2
        if dst.type == 'mem':
            base += self._modrm_extra_size(dst)
        elif src.type == 'mem':
            base += self._modrm_extra_size(src)
        return base

    def _estimate_mov_size(self, pl: ParsedLine, prefix_size: int) -> int:
        ops = pl.operands
        if len(ops) != 2:
            return prefix_size + 6
        dst, src = ops[0], ops[1]

        # MOV r32, imm32
        if dst.type == 'reg' and src.type in ('imm', 'label'):
            if dst.reg in REGS_8:
                return prefix_size + 2  # B0+r imm8
            if dst.reg in REGS_16:
                return prefix_size + 4  # 66h B8+r imm16
            return prefix_size + 5  # B8+r imm32

        # MOV r/m, r or MOV r, r/m
        if (dst.type == 'reg' and src.type == 'reg'):
            if dst.reg in REGS_16 or src.reg in REGS_16:
                return prefix_size + 3  # 66h prefix
            return prefix_size + 2

        # MOV with memory
        base = prefix_size + 2  # opcode + modrm
        mem_op = dst if dst.type == 'mem' else src if src.type == 'mem' else None
        if mem_op:
            base += self._modrm_extra_size(mem_op)
        # 16-bit register needs 66h prefix
        reg_op = dst if dst.type == 'reg' else src if src.type == 'reg' else None
        if reg_op and reg_op.reg in REGS_16:
            base += 1  # 66h prefix
        if src.type in ('imm', 'label'):
            if dst.size_hint == 8:
                base += 1
            elif dst.size_hint == 16:
                base += 2 + 1  # 66h prefix + imm16
            else:
                base += 4
        return base

    # -----------------------------------------------------------------------
    # Pass 2: Code generation
    # -----------------------------------------------------------------------

    def _pass2(self):
        self.current_address = self.origin

        for pl in self.lines:
            pl.address = self.current_address

            if pl.directive:
                code = self._encode_directive(pl)
                pl.machine_code = code
                self.current_address += len(code)
                continue

            if not pl.mnemonic:
                continue

            try:
                code = self._encode_instruction(pl)
            except Exception as e:
                code = b'\x90' * pl.size  # NOP padding on error
                pl.error = str(e)
                self.errors.append(f"Line {pl.line_num}: {e}")

            # Handle size mismatch
            if len(code) != pl.size:
                if len(code) < pl.size:
                    code = code + b'\x90' * (pl.size - len(code))
                else:
                    pl.error = f"Size mismatch: expected {pl.size}, got {len(code)}"
                    self.errors.append(f"Line {pl.line_num}: {pl.error}")
                    code = code[:pl.size]

            pl.machine_code = code
            self.current_address += pl.size

    def _encode_directive(self, pl: ParsedLine) -> bytes:
        d = pl.directive
        if d in ('ORG', 'SECTION', 'EQU', 'GLOBAL', 'EXTERN', 'BITS', 'TIMES'):
            return b''
        if d == 'DB':
            return self._encode_data(pl.directive_args, 1)
        if d == 'DW':
            return self._encode_data(pl.directive_args, 2)
        if d == 'DD':
            return self._encode_data(pl.directive_args, 4)
        if d == 'DQ':
            return self._encode_data(pl.directive_args, 8)
        if d == 'RESB':
            return b'\x00' * self._eval_expr(pl.directive_args)
        if d == 'RESW':
            return b'\x00' * (self._eval_expr(pl.directive_args) * 2)
        if d == 'RESD':
            return b'\x00' * (self._eval_expr(pl.directive_args) * 4)
        return b''

    def _encode_data(self, args: str, elem_size: int) -> bytes:
        items = self._split_data_args(args)
        result = bytearray()
        for item in items:
            item = item.strip()
            if not item:
                continue
            if (item.startswith('"') and item.endswith('"')) or \
               (item.startswith("'") and item.endswith("'")):
                data = self._parse_string_literal(item)
                if elem_size == 1:
                    result.extend(data)
                else:
                    for b in data:
                        result.extend(struct.pack('<' + {2: 'H', 4: 'I', 8: 'Q'}[elem_size], b))
            elif item == '?':
                result.extend(b'\x00' * elem_size)
            else:
                val = self._eval_expr(item)
                if elem_size == 1:
                    result.append(val & 0xFF)
                elif elem_size == 2:
                    result.extend(struct.pack('<H', val & 0xFFFF))
                elif elem_size == 4:
                    result.extend(struct.pack('<I', val & 0xFFFFFFFF))
                elif elem_size == 8:
                    result.extend(struct.pack('<Q', val & 0xFFFFFFFFFFFFFFFF))
        return bytes(result)

    # -----------------------------------------------------------------------
    # Instruction encoding
    # -----------------------------------------------------------------------

    def _encode_instruction(self, pl: ParsedLine) -> bytes:
        mn = pl.mnemonic
        ops = pl.operands

        prefix = b''
        if pl.prefix:
            prefix = bytes([PREFIX_BYTES[pl.prefix]])

        # Resolve label operands to immediates
        for op in ops:
            if op.type == 'label' and op.mem_disp_label:
                if op.mem_disp_label in self.symbols:
                    op.imm_val = self.symbols[op.mem_disp_label]
                    op.type = 'imm'
                else:
                    raise ValueError(f"Undefined symbol: {op.mem_disp_label}")
            if op.type == 'mem' and op.mem_disp_label:
                if op.mem_disp_label in self.symbols:
                    op.mem_disp += self.symbols[op.mem_disp_label]
                    op.mem_disp_label = ''
                else:
                    raise ValueError(f"Undefined symbol: {op.mem_disp_label}")

        # Simple instructions
        if mn in SIMPLE_OPCODES and len(ops) == 0:
            return prefix + SIMPLE_OPCODES[mn]

        # INT
        if mn == 'INT':
            if len(ops) == 1:
                val = ops[0].imm_val if ops[0].type == 'imm' else 0
                if val == 3:
                    return prefix + b'\xCC'
                return prefix + b'\xCD' + bytes([val & 0xFF])

        # RET with immediate
        if mn == 'RET':
            if len(ops) == 0:
                return prefix + b'\xC3'
            if len(ops) == 1:
                val = ops[0].imm_val & 0xFFFF
                return prefix + b'\xC2' + struct.pack('<H', val)

        # PUSH
        if mn == 'PUSH':
            return prefix + self._encode_push(ops[0])

        # POP
        if mn == 'POP':
            return prefix + self._encode_pop(ops[0])

        # MOV
        if mn == 'MOV':
            return prefix + self._encode_mov(ops[0], ops[1])

        # XCHG
        if mn == 'XCHG':
            return prefix + self._encode_xchg(ops[0], ops[1])

        # LEA
        if mn == 'LEA':
            return prefix + self._encode_lea(ops[0], ops[1])

        # MOVZX / MOVSX
        if mn == 'MOVZX':
            return prefix + self._encode_movzx_sx(ops[0], ops[1], 0xB6)
        if mn == 'MOVSX':
            return prefix + self._encode_movzx_sx(ops[0], ops[1], 0xBE)

        # ALU ops
        if mn in ALU_OPS:
            return prefix + self._encode_alu(mn, ops[0], ops[1])

        # TEST
        if mn == 'TEST':
            return prefix + self._encode_test(ops[0], ops[1])

        # INC/DEC
        if mn == 'INC':
            return prefix + self._encode_inc_dec(ops[0], 0)
        if mn == 'DEC':
            return prefix + self._encode_inc_dec(ops[0], 1)

        # NEG/NOT
        if mn == 'NEG':
            return prefix + self._encode_unary_f7(ops[0], 3)
        if mn == 'NOT':
            return prefix + self._encode_unary_f7(ops[0], 2)

        # MUL/IMUL/DIV/IDIV
        if mn == 'MUL':
            return prefix + self._encode_unary_f7(ops[0], 4)
        if mn == 'DIV':
            return prefix + self._encode_unary_f7(ops[0], 6)
        if mn == 'IDIV':
            return prefix + self._encode_unary_f7(ops[0], 7)
        if mn == 'IMUL':
            return prefix + self._encode_imul(ops)

        # Shifts/Rotates
        if mn in SHIFT_OPS:
            return prefix + self._encode_shift(mn, ops)

        # JMP
        if mn == 'JMP':
            return prefix + self._encode_jmp(pl, ops[0])

        # Jcc
        if mn in JCC_CODES:
            return prefix + self._encode_jcc(pl, mn, ops[0])

        # CALL
        if mn == 'CALL':
            return prefix + self._encode_call(pl, ops[0])

        # LOOP variants
        if mn in ('LOOP', 'LOOPE', 'LOOPZ', 'LOOPNE', 'LOOPNZ'):
            return prefix + self._encode_loop(pl, mn, ops[0])

        # JECXZ
        if mn == 'JECXZ':
            return prefix + self._encode_jecxz(pl, ops[0])

        raise ValueError(f"Unknown instruction: {mn}")

    # -----------------------------------------------------------------------
    # PUSH / POP
    # -----------------------------------------------------------------------

    def _encode_push(self, op: Operand) -> bytes:
        if op.type == 'reg':
            r = op.reg.upper()
            if r in REGS_32:
                return bytes([0x50 + REGS_32[r]])
            if r in REGS_16:
                return bytes([0x66, 0x50 + REGS_16[r]])
            if r in SEGMENT_REGS:
                seg = r
                if seg == 'CS': return b'\x0E'
                if seg == 'SS': return b'\x16'
                if seg == 'DS': return b'\x1E'
                if seg == 'ES': return b'\x06'
                if seg == 'FS': return b'\x0F\xA0'
                if seg == 'GS': return b'\x0F\xA8'
        if op.type == 'imm':
            val = op.imm_val
            if -128 <= val <= 127:
                return bytes([0x6A, val & 0xFF])
            return b'\x68' + _pack_i32(val)
        if op.type == 'mem':
            modrm, extra = self._encode_modrm(op, 6)  # /6
            return b'\xFF' + modrm + extra
        raise ValueError(f"Invalid PUSH operand: {op.raw}")

    def _encode_pop(self, op: Operand) -> bytes:
        if op.type == 'reg':
            r = op.reg.upper()
            if r in REGS_32:
                return bytes([0x58 + REGS_32[r]])
            if r in REGS_16:
                return bytes([0x66, 0x58 + REGS_16[r]])
            if r in SEGMENT_REGS:
                seg = r
                if seg == 'SS': return b'\x17'
                if seg == 'DS': return b'\x1F'
                if seg == 'ES': return b'\x07'
                if seg == 'FS': return b'\x0F\xA1'
                if seg == 'GS': return b'\x0F\xA9'
        if op.type == 'mem':
            modrm, extra = self._encode_modrm(op, 0)  # /0
            return b'\x8F' + modrm + extra
        raise ValueError(f"Invalid POP operand: {op.raw}")

    # -----------------------------------------------------------------------
    # MOV
    # -----------------------------------------------------------------------

    def _encode_mov(self, dst: Operand, src: Operand) -> bytes:
        # MOV r32, imm32
        if dst.type == 'reg' and src.type == 'imm':
            r = dst.reg.upper()
            val = src.imm_val
            if r in REGS_8:
                return bytes([0xB0 + REGS_8[r], val & 0xFF])
            if r in REGS_16:
                return bytes([0x66, 0xB8 + REGS_16[r]]) + struct.pack('<H', val & 0xFFFF)
            if r in REGS_32:
                return bytes([0xB8 + REGS_32[r]]) + struct.pack('<I', val & 0xFFFFFFFF)

        # MOV r/m, r (store)
        if dst.type == 'mem' and src.type == 'reg':
            r = src.reg.upper()
            if r in REGS_8:
                modrm, extra = self._encode_modrm(dst, REGS_8[r])
                return b'\x88' + modrm + extra
            if r in REGS_16:
                modrm, extra = self._encode_modrm(dst, REGS_16[r])
                return b'\x66\x89' + modrm + extra
            if r in REGS_32:
                modrm, extra = self._encode_modrm(dst, REGS_32[r])
                return b'\x89' + modrm + extra

        # MOV r, r/m (load)
        if dst.type == 'reg' and src.type in ('mem', 'reg'):
            r = dst.reg.upper()
            if src.type == 'reg':
                s = src.reg.upper()
                if r in REGS_8 and s in REGS_8:
                    return bytes([0x8A, 0xC0 | (REGS_8[r] << 3) | REGS_8[s]])
                if r in REGS_16 and s in REGS_16:
                    return bytes([0x66, 0x8B, 0xC0 | (REGS_16[r] << 3) | REGS_16[s]])
                if r in REGS_32 and s in REGS_32:
                    return bytes([0x8B, 0xC0 | (REGS_32[r] << 3) | REGS_32[s]])
            else:  # mem
                if r in REGS_8:
                    modrm, extra = self._encode_modrm(src, REGS_8[r])
                    return b'\x8A' + modrm + extra
                if r in REGS_16:
                    modrm, extra = self._encode_modrm(src, REGS_16[r])
                    return b'\x66\x8B' + modrm + extra
                if r in REGS_32:
                    modrm, extra = self._encode_modrm(src, REGS_32[r])
                    return b'\x8B' + modrm + extra

        # MOV r/m, imm
        if dst.type == 'mem' and src.type == 'imm':
            val = src.imm_val
            size = dst.size_hint or 32
            if size == 8:
                modrm, extra = self._encode_modrm(dst, 0)
                return b'\xC6' + modrm + extra + bytes([val & 0xFF])
            if size == 16:
                modrm, extra = self._encode_modrm(dst, 0)
                return b'\x66\xC7' + modrm + extra + struct.pack('<H', val & 0xFFFF)
            # 32-bit
            modrm, extra = self._encode_modrm(dst, 0)
            return b'\xC7' + modrm + extra + struct.pack('<I', val & 0xFFFFFFFF)

        # MOV r, r (same size handled above, but catch remaining)
        if dst.type == 'reg' and src.type == 'reg':
            # Handle segment register moves
            s = src.reg.upper()
            d = dst.reg.upper()
            if s in SEGMENT_REGS:
                modrm = bytes([0xC0 | (SEGMENT_REGS[s] << 3) | REGS_32.get(d, REGS_16.get(d, 0))])
                return b'\x8C' + modrm
            if d in SEGMENT_REGS:
                modrm = bytes([0xC0 | (SEGMENT_REGS[d] << 3) | REGS_32.get(s, REGS_16.get(s, 0))])
                return b'\x8E' + modrm

        raise ValueError(f"Invalid MOV operands: {dst.raw}, {src.raw}")

    # -----------------------------------------------------------------------
    # XCHG
    # -----------------------------------------------------------------------

    def _encode_xchg(self, dst: Operand, src: Operand) -> bytes:
        if dst.type == 'reg' and src.type == 'reg':
            d, s = dst.reg, src.reg
            if d in REGS_32 and s in REGS_32:
                if d == 'EAX':
                    return bytes([0x90 + REGS_32[s]])
                if s == 'EAX':
                    return bytes([0x90 + REGS_32[d]])
                return bytes([0x87, 0xC0 | (REGS_32[d] << 3) | REGS_32[s]])
            if d in REGS_8 and s in REGS_8:
                return bytes([0x86, 0xC0 | (REGS_8[d] << 3) | REGS_8[s]])
        if dst.type == 'reg' and src.type == 'mem':
            modrm, extra = self._encode_modrm(src, REGS_32.get(dst.reg, 0))
            return b'\x87' + modrm + extra
        if dst.type == 'mem' and src.type == 'reg':
            modrm, extra = self._encode_modrm(dst, REGS_32.get(src.reg, 0))
            return b'\x87' + modrm + extra
        raise ValueError(f"Invalid XCHG operands")

    # -----------------------------------------------------------------------
    # LEA
    # -----------------------------------------------------------------------

    def _encode_lea(self, dst: Operand, src: Operand) -> bytes:
        if dst.type != 'reg' or src.type != 'mem':
            raise ValueError("LEA requires reg, mem operands")
        r_code = REGS_32.get(dst.reg, 0)
        modrm, extra = self._encode_modrm(src, r_code)
        return b'\x8D' + modrm + extra

    # -----------------------------------------------------------------------
    # MOVZX / MOVSX
    # -----------------------------------------------------------------------

    def _encode_movzx_sx(self, dst: Operand, src: Operand, base: int) -> bytes:
        r_code = REGS_32.get(dst.reg, REGS_16.get(dst.reg, 0))
        src_size = src.size_hint or (8 if src.type == 'reg' and src.reg in REGS_8 else 16)

        if src_size == 8:
            opcode = bytes([0x0F, base])
        else:
            opcode = bytes([0x0F, base + 1])

        if src.type == 'reg':
            src_code = reg_code(src.reg)
            return opcode + bytes([0xC0 | (r_code << 3) | src_code])
        else:
            modrm, extra = self._encode_modrm(src, r_code)
            return opcode + modrm + extra

    # -----------------------------------------------------------------------
    # ALU ops (ADD, SUB, AND, OR, XOR, CMP, ADC, SBB)
    # -----------------------------------------------------------------------

    def _encode_alu(self, mn: str, dst: Operand, src: Operand) -> bytes:
        base_opcode, digit = ALU_OPS[mn]

        # r/m, imm
        if src.type == 'imm':
            val = src.imm_val

            # AL, imm8 short form
            if dst.type == 'reg' and dst.reg == 'AL':
                return bytes([base_opcode + 4, val & 0xFF])

            # AX, imm16
            if dst.type == 'reg' and dst.reg == 'AX':
                return bytes([0x66, base_opcode + 5]) + struct.pack('<H', val & 0xFFFF)

            # EAX, imm32 short form (but try imm8 first)
            if dst.type == 'reg' and dst.reg == 'EAX':
                if -128 <= val <= 127:
                    return bytes([0x83, 0xC0 | (digit << 3) | REGS_32['EAX'], val & 0xFF])
                return bytes([base_opcode + 5]) + _pack_i32(val)

            # r/m32, imm8
            if -128 <= val <= 127:
                if dst.type == 'reg':
                    r = dst.reg
                    if r in REGS_32:
                        return bytes([0x83, 0xC0 | (digit << 3) | REGS_32[r], val & 0xFF])
                    if r in REGS_8:
                        return bytes([0x80, 0xC0 | (digit << 3) | REGS_8[r], val & 0xFF])
                if dst.type == 'mem':
                    size = dst.size_hint or 32
                    if size == 8:
                        modrm, extra = self._encode_modrm(dst, digit)
                        return b'\x80' + modrm + extra + bytes([val & 0xFF])
                    modrm, extra = self._encode_modrm(dst, digit)
                    return b'\x83' + modrm + extra + bytes([val & 0xFF])

            # r/m32, imm32
            if dst.type == 'reg':
                r = dst.reg
                if r in REGS_32:
                    return bytes([0x81, 0xC0 | (digit << 3) | REGS_32[r]]) + _pack_i32(val)
                if r in REGS_8:
                    return bytes([0x80, 0xC0 | (digit << 3) | REGS_8[r], val & 0xFF])
            if dst.type == 'mem':
                size = dst.size_hint or 32
                if size == 8:
                    modrm, extra = self._encode_modrm(dst, digit)
                    return b'\x80' + modrm + extra + bytes([val & 0xFF])
                modrm, extra = self._encode_modrm(dst, digit)
                return b'\x81' + modrm + extra + _pack_i32(val)

        # r, r/m (load direction)
        if dst.type == 'reg' and src.type in ('reg', 'mem'):
            r = dst.reg
            if r in REGS_8:
                opcode = base_opcode + 2
                if src.type == 'reg':
                    return bytes([opcode, 0xC0 | (REGS_8[r] << 3) | REGS_8[src.reg]])
                modrm, extra = self._encode_modrm(src, REGS_8[r])
                return bytes([opcode]) + modrm + extra

            if r in REGS_32:
                opcode = base_opcode + 3
                if src.type == 'reg':
                    return bytes([opcode, 0xC0 | (REGS_32[r] << 3) | REGS_32[src.reg]])
                modrm, extra = self._encode_modrm(src, REGS_32[r])
                return bytes([opcode]) + modrm + extra

        # r/m, r (store direction)
        if dst.type == 'mem' and src.type == 'reg':
            r = src.reg
            if r in REGS_8:
                opcode = base_opcode
                modrm, extra = self._encode_modrm(dst, REGS_8[r])
                return bytes([opcode]) + modrm + extra
            if r in REGS_32:
                opcode = base_opcode + 1
                modrm, extra = self._encode_modrm(dst, REGS_32[r])
                return bytes([opcode]) + modrm + extra

        raise ValueError(f"Invalid {mn} operands: {dst.raw}, {src.raw}")

    # -----------------------------------------------------------------------
    # TEST
    # -----------------------------------------------------------------------

    def _encode_test(self, dst: Operand, src: Operand) -> bytes:
        if src.type == 'imm':
            val = src.imm_val
            if dst.type == 'reg':
                r = dst.reg
                if r == 'AL':
                    return bytes([0xA8, val & 0xFF])
                if r == 'EAX':
                    return b'\xA9' + struct.pack('<I', val & 0xFFFFFFFF)
                if r in REGS_8:
                    return bytes([0xF6, 0xC0 | REGS_8[r], val & 0xFF])
                if r in REGS_32:
                    return bytes([0xF7, 0xC0 | REGS_32[r]]) + struct.pack('<I', val & 0xFFFFFFFF)
            if dst.type == 'mem':
                size = dst.size_hint or 32
                if size == 8:
                    modrm, extra = self._encode_modrm(dst, 0)
                    return b'\xF6' + modrm + extra + bytes([val & 0xFF])
                modrm, extra = self._encode_modrm(dst, 0)
                return b'\xF7' + modrm + extra + struct.pack('<I', val & 0xFFFFFFFF)

        # TEST r/m, r
        if src.type == 'reg':
            r = src.reg
            if r in REGS_8:
                if dst.type == 'reg':
                    return bytes([0x84, 0xC0 | (REGS_8[r] << 3) | REGS_8.get(dst.reg, 0)])
                modrm, extra = self._encode_modrm(dst, REGS_8[r])
                return b'\x84' + modrm + extra
            if r in REGS_32:
                if dst.type == 'reg':
                    return bytes([0x85, 0xC0 | (REGS_32[r] << 3) | REGS_32.get(dst.reg, 0)])
                modrm, extra = self._encode_modrm(dst, REGS_32[r])
                return b'\x85' + modrm + extra

        raise ValueError(f"Invalid TEST operands")

    # -----------------------------------------------------------------------
    # INC / DEC
    # -----------------------------------------------------------------------

    def _encode_inc_dec(self, op: Operand, is_dec: int) -> bytes:
        if op.type == 'reg':
            r = op.reg
            if r in REGS_32:
                return bytes([0x40 + is_dec * 8 + REGS_32[r]])
            if r in REGS_16:
                return bytes([0x66, 0x40 + is_dec * 8 + REGS_16[r]])
            if r in REGS_8:
                return bytes([0xFE, 0xC0 | (is_dec << 3) | REGS_8[r]])
        if op.type == 'mem':
            size = op.size_hint or 32
            digit = is_dec
            if size == 8:
                modrm, extra = self._encode_modrm(op, digit)
                return b'\xFE' + modrm + extra
            modrm, extra = self._encode_modrm(op, digit)
            return b'\xFF' + modrm + extra
        raise ValueError(f"Invalid INC/DEC operand")

    # -----------------------------------------------------------------------
    # NEG / NOT / MUL / DIV / IDIV (unary F6/F7 group)
    # -----------------------------------------------------------------------

    def _encode_unary_f7(self, op: Operand, digit: int) -> bytes:
        if op.type == 'reg':
            r = op.reg
            if r in REGS_8:
                return bytes([0xF6, 0xC0 | (digit << 3) | REGS_8[r]])
            if r in REGS_32:
                return bytes([0xF7, 0xC0 | (digit << 3) | REGS_32[r]])
            if r in REGS_16:
                return bytes([0x66, 0xF7, 0xC0 | (digit << 3) | REGS_16[r]])
        if op.type == 'mem':
            size = op.size_hint or 32
            if size == 8:
                modrm, extra = self._encode_modrm(op, digit)
                return b'\xF6' + modrm + extra
            modrm, extra = self._encode_modrm(op, digit)
            return b'\xF7' + modrm + extra
        raise ValueError(f"Invalid unary operand: {op.raw}")

    # -----------------------------------------------------------------------
    # IMUL (multi-form)
    # -----------------------------------------------------------------------

    def _encode_imul(self, ops: List[Operand]) -> bytes:
        if len(ops) == 1:
            return self._encode_unary_f7(ops[0], 5)

        if len(ops) == 2:
            # IMUL r, r/m
            dst, src = ops[0], ops[1]
            r_code = REGS_32.get(dst.reg, 0)
            if src.type == 'reg':
                s_code = REGS_32.get(src.reg, 0)
                return bytes([0x0F, 0xAF, 0xC0 | (r_code << 3) | s_code])
            if src.type == 'mem':
                modrm, extra = self._encode_modrm(src, r_code)
                return bytes([0x0F, 0xAF]) + modrm + extra

        if len(ops) == 3:
            # IMUL r, r/m, imm
            dst, src, imm_op = ops[0], ops[1], ops[2]
            r_code = REGS_32.get(dst.reg, 0)
            val = imm_op.imm_val
            if src.type == 'reg':
                s_code = REGS_32.get(src.reg, 0)
                if -128 <= val <= 127:
                    return bytes([0x6B, 0xC0 | (r_code << 3) | s_code, val & 0xFF])
                return bytes([0x69, 0xC0 | (r_code << 3) | s_code]) + _pack_i32(val)
            if src.type == 'mem':
                modrm, extra = self._encode_modrm(src, r_code)
                if -128 <= val <= 127:
                    return b'\x6B' + modrm + extra + bytes([val & 0xFF])
                return b'\x69' + modrm + extra + _pack_i32(val)

        raise ValueError("Invalid IMUL operands")

    # -----------------------------------------------------------------------
    # Shifts / Rotates
    # -----------------------------------------------------------------------

    def _encode_shift(self, mn: str, ops: List[Operand]) -> bytes:
        digit = SHIFT_OPS[mn]
        dst = ops[0]

        # Determine count
        if len(ops) >= 2:
            count_op = ops[1]
        else:
            count_op = Operand(type='imm', imm_val=1)

        # Determine opcode base by size
        is_8bit = (dst.type == 'reg' and dst.reg in REGS_8) or dst.size_hint == 8

        if count_op.type == 'imm' and count_op.imm_val == 1:
            opcode = 0xD0 if is_8bit else 0xD1
            if dst.type == 'reg':
                code = REGS_8.get(dst.reg, REGS_32.get(dst.reg, REGS_16.get(dst.reg, 0)))
                return bytes([opcode, 0xC0 | (digit << 3) | code])
            modrm, extra = self._encode_modrm(dst, digit)
            return bytes([opcode]) + modrm + extra

        if count_op.type == 'reg' and count_op.reg == 'CL':
            opcode = 0xD2 if is_8bit else 0xD3
            if dst.type == 'reg':
                code = REGS_8.get(dst.reg, REGS_32.get(dst.reg, REGS_16.get(dst.reg, 0)))
                return bytes([opcode, 0xC0 | (digit << 3) | code])
            modrm, extra = self._encode_modrm(dst, digit)
            return bytes([opcode]) + modrm + extra

        # imm8
        opcode = 0xC0 if is_8bit else 0xC1
        val = count_op.imm_val & 0xFF
        if dst.type == 'reg':
            code = REGS_8.get(dst.reg, REGS_32.get(dst.reg, REGS_16.get(dst.reg, 0)))
            return bytes([opcode, 0xC0 | (digit << 3) | code, val])
        modrm, extra = self._encode_modrm(dst, digit)
        return bytes([opcode]) + modrm + extra + bytes([val])

    # -----------------------------------------------------------------------
    # JMP
    # -----------------------------------------------------------------------

    def _encode_jmp(self, pl: ParsedLine, op: Operand) -> bytes:
        if op.type == 'imm':
            target = op.imm_val
            rel32 = target - (pl.address + pl.size)
            return b'\xE9' + _pack_disp32(rel32)

        if op.type == 'reg':
            r = op.reg
            if r in REGS_32:
                return bytes([0xFF, 0xE0 | REGS_32[r]])

        if op.type == 'mem':
            modrm, extra = self._encode_modrm(op, 4)  # /4
            return b'\xFF' + modrm + extra

        raise ValueError(f"Invalid JMP operand: {op.raw}")

    # -----------------------------------------------------------------------
    # Jcc (conditional jumps)
    # -----------------------------------------------------------------------

    def _encode_jcc(self, pl: ParsedLine, mn: str, op: Operand) -> bytes:
        cc = JCC_CODES[mn]
        target = op.imm_val
        rel32 = target - (pl.address + pl.size)
        return bytes([0x0F, 0x80 + cc]) + _pack_disp32(rel32)

    # -----------------------------------------------------------------------
    # CALL
    # -----------------------------------------------------------------------

    def _encode_call(self, pl: ParsedLine, op: Operand) -> bytes:
        if op.type == 'imm':
            target = op.imm_val
            rel32 = target - (pl.address + pl.size)
            return b'\xE8' + _pack_disp32(rel32)

        if op.type == 'reg':
            r = op.reg
            if r in REGS_32:
                return bytes([0xFF, 0xD0 | REGS_32[r]])

        if op.type == 'mem':
            modrm, extra = self._encode_modrm(op, 2)  # /2
            return b'\xFF' + modrm + extra

        raise ValueError(f"Invalid CALL operand: {op.raw}")

    # -----------------------------------------------------------------------
    # LOOP / LOOPE / LOOPNE
    # -----------------------------------------------------------------------

    def _encode_loop(self, pl: ParsedLine, mn: str, op: Operand) -> bytes:
        opcodes = {
            'LOOP': 0xE2, 'LOOPE': 0xE1, 'LOOPZ': 0xE1,
            'LOOPNE': 0xE0, 'LOOPNZ': 0xE0,
        }
        target = op.imm_val
        rel8 = target - (pl.address + 2)
        if rel8 < -128 or rel8 > 127:
            raise ValueError(f"LOOP target out of range: {rel8}")
        return bytes([opcodes[mn], rel8 & 0xFF])

    # -----------------------------------------------------------------------
    # JECXZ
    # -----------------------------------------------------------------------

    def _encode_jecxz(self, pl: ParsedLine, op: Operand) -> bytes:
        target = op.imm_val
        rel8 = target - (pl.address + 2)
        if rel8 < -128 or rel8 > 127:
            raise ValueError(f"JECXZ target out of range")
        return bytes([0xE3, rel8 & 0xFF])

    # -----------------------------------------------------------------------
    # ModR/M + SIB byte encoding
    # -----------------------------------------------------------------------

    def _encode_modrm(self, mem_op: Operand, reg_field: int) -> Tuple[bytes, bytes]:
        """
        Encode ModR/M (and SIB if needed) for a memory operand.
        Returns (modrm_byte, extra_bytes) where extra includes SIB and displacement.
        """
        base = mem_op.mem_base
        index = mem_op.mem_index
        scale = mem_op.mem_scale
        disp = mem_op.mem_disp

        reg_field &= 7

        # Direct memory addressing (no base, no index) - [disp32]
        if not base and not index:
            modrm = bytes([(reg_field << 3) | 0x05])
            return modrm, _pack_disp32(disp)

        # SIB with no base (index*scale + disp32)
        if not base and index:
            if index == 'ESP':
                raise ValueError("ESP cannot be used as index register")
            index_code = REGS_32.get(index, 0)
            scale_bits = {1: 0, 2: 1, 4: 2, 8: 3}[scale]
            modrm = bytes([(reg_field << 3) | 0x04])  # mod=00, rm=100 (SIB follows)
            sib = bytes([(scale_bits << 6) | (index_code << 3) | 0x05])  # base=101 (none)
            return modrm, sib + _pack_disp32(disp)

        base_code = REGS_32.get(base, 0)

        # Determine if SIB byte is needed
        need_sib = bool(index) or base == 'ESP'

        # Determine displacement mode
        if base == 'EBP' and disp == 0:
            mod = 1  # [EBP] requires disp8=0
            disp_bytes = b'\x00'
        elif disp == 0:
            mod = 0
            disp_bytes = b''
        elif -128 <= disp <= 127:
            mod = 1
            disp_bytes = struct.pack('<b', disp)
        else:
            mod = 2
            disp_bytes = _pack_disp32(disp)

        if need_sib:
            rm = 4  # SIB follows
            modrm = bytes([(mod << 6) | (reg_field << 3) | rm])

            if index and index != 'ESP':
                index_code = REGS_32.get(index, 0)
            else:
                index_code = 4  # no index (ESP placeholder)

            scale_bits = {1: 0, 2: 1, 4: 2, 8: 3}.get(scale, 0)
            sib = bytes([(scale_bits << 6) | (index_code << 3) | base_code])
            return modrm, sib + disp_bytes
        else:
            modrm = bytes([(mod << 6) | (reg_field << 3) | base_code])
            return modrm, disp_bytes

    # -----------------------------------------------------------------------
    # Listing generation
    # -----------------------------------------------------------------------

    def _generate_listing(self) -> str:
        lines = []
        lines.append("=" * 78)
        lines.append("  x86-32 Assembly Listing")
        lines.append("=" * 78)
        lines.append("")
        lines.append(f"{'Line':>5}  {'Address':>8}  {'Machine Code':<24}  Source")
        lines.append("-" * 78)

        for pl in self.lines:
            line_str = f"{pl.line_num:5d}"

            if pl.mnemonic or pl.directive in ('DB', 'DW', 'DD', 'DQ', 'RESB', 'RESW', 'RESD'):
                addr_str = f"{pl.address:08X}"
                hex_bytes = ' '.join(f'{b:02X}' for b in pl.machine_code)
                if len(hex_bytes) > 24:
                    hex_bytes = hex_bytes[:21] + '...'
                hex_str = f"{hex_bytes:<24}"
            elif pl.is_equ:
                addr_str = f"={pl.equ_value:08X}" if pl.label else "        "
                hex_str = " " * 24
            else:
                addr_str = "        "
                hex_str = " " * 24

            raw = pl.raw_text
            error_str = f"  *** ERROR: {pl.error}" if pl.error else ""

            lines.append(f"{line_str}  {addr_str}  {hex_str}  {raw}{error_str}")

        # Symbol table
        if self.symbols:
            lines.append("")
            lines.append("=" * 78)
            lines.append("  Symbol Table")
            lines.append("=" * 78)
            lines.append(f"  {'Symbol':<30}  {'Value':>10}")
            lines.append("  " + "-" * 42)
            for sym, val in sorted(self.symbols.items()):
                lines.append(f"  {sym:<30}  {val:>10}  (0x{val:08X})")

        # Error summary
        if self.errors:
            lines.append("")
            lines.append("=" * 78)
            lines.append(f"  Errors: {len(self.errors)}")
            lines.append("=" * 78)
            for err in self.errors:
                lines.append(f"  {err}")
        else:
            lines.append("")
            lines.append("=" * 78)
            lines.append("  Assembly completed successfully")
            lines.append(f"  Total bytes: {sum(pl.size for pl in self.lines)}")
            lines.append("=" * 78)

        return '\n'.join(lines)
