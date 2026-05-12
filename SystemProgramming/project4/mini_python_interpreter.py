"""Міні-інтерпретатор ~Python для лабораторної роботи 12.

Реалізовані елементи мови:
- числові та логічні літерали;
- арифметичні вирази: +, -, *, /, //, %, **, унарні + і -;
- логічні вирази: and, or, not, порівняння;
- вбудовані функції: sin(), cos(), int(), float(), input();
- оператори: присвоєння, print(), if-else, while;
- блоки операторів з відступами ліворуч.

Інтерпретатор побудований у класичній схемі:
1) лексичний аналіз із виділенням INDENT/DEDENT;
2) рекурсивний спуск за граматикою;
3) окремий етап виконання побудованого синтаксичного дерева.
"""

from __future__ import annotations

from dataclasses import dataclass
import math
import pathlib
import re
import sys
from typing import Any, Iterable, TextIO


class MiniPythonError(Exception):
    """Базовий клас помилок інтерпретатора."""


class MiniPythonSyntaxError(MiniPythonError):
    """Синтаксична помилка під час сканування або розбору."""


class MiniPythonRuntimeError(MiniPythonError):
    """Помилка під час виконання програми."""


@dataclass(frozen=True)
class Token:
    kind: str
    value: Any
    line: int
    column: int


@dataclass(frozen=True)
class Number:
    value: float | int


@dataclass(frozen=True)
class Boolean:
    value: bool


@dataclass(frozen=True)
class Variable:
    name: str


@dataclass(frozen=True)
class UnaryOp:
    operator: str
    operand: Any


@dataclass(frozen=True)
class BinaryOp:
    left: Any
    operator: str
    right: Any


@dataclass(frozen=True)
class Call:
    name: str
    arguments: list[Any]


@dataclass(frozen=True)
class AssignStmt:
    name: str
    expression: Any


@dataclass(frozen=True)
class PrintStmt:
    expressions: list[Any]


@dataclass(frozen=True)
class IfStmt:
    condition: Any
    then_block: list[Any]
    else_block: list[Any] | None


@dataclass(frozen=True)
class WhileStmt:
    condition: Any
    body: list[Any]


@dataclass(frozen=True)
class Program:
    statements: list[Any]


class Lexer:
    """Перетворює текст програми у список лексем."""

    _number_pattern = re.compile(r"\d+(?:\.\d+)?(?:[eE][+-]?\d+)?")
    _name_pattern = re.compile(r"[A-Za-z_][A-Za-z_0-9]*")
    _keywords = {"if", "else", "while", "and", "or", "not", "print", "True", "False"}
    _multi_char_ops = ("//", "**", "==", "!=", "<=", ">=")
    _single_char_tokens = {
        "(": "LPAREN",
        ")": "RPAREN",
        ":": "COLON",
        ",": "COMMA",
        "+": "OP",
        "-": "OP",
        "*": "OP",
        "/": "OP",
        "%": "OP",
        "=": "ASSIGN",
        "<": "OP",
        ">": "OP",
    }

    def __init__(self, text: str):
        self.text = text

    def tokenize(self) -> list[Token]:
        tokens: list[Token] = []
        indent_stack = [0]
        lines = self.text.splitlines()

        for line_no, raw_line in enumerate(lines, start=1):
            if "\t" in raw_line:
                raise MiniPythonSyntaxError(
                    f"Рядок {line_no}: табуляція не підтримується, використовуйте пробіли."
                )

            logical_line = raw_line.split("#", 1)[0]
            if logical_line.strip() == "":
                continue

            indent = len(logical_line) - len(logical_line.lstrip(" "))
            content = logical_line[indent:]

            if indent > indent_stack[-1]:
                indent_stack.append(indent)
                tokens.append(Token("INDENT", indent, line_no, 1))
            else:
                while indent < indent_stack[-1]:
                    indent_stack.pop()
                    tokens.append(Token("DEDENT", indent, line_no, 1))
                if indent != indent_stack[-1]:
                    raise MiniPythonSyntaxError(
                        f"Рядок {line_no}: неправильний рівень відступу."
                    )

            tokens.extend(self._tokenize_content(content, line_no, indent))
            tokens.append(Token("NEWLINE", "\\n", line_no, len(logical_line) + 1))

        while len(indent_stack) > 1:
            indent_stack.pop()
            tokens.append(Token("DEDENT", 0, len(lines) + 1, 1))

        tokens.append(Token("EOF", "#", len(lines) + 1, 1))
        return tokens

    def _tokenize_content(self, content: str, line_no: int, indent: int) -> list[Token]:
        tokens: list[Token] = []
        i = 0

        while i < len(content):
            char = content[i]
            column = indent + i + 1

            if char == " ":
                i += 1
                continue

            matched_op = next((op for op in self._multi_char_ops if content.startswith(op, i)), None)
            if matched_op is not None:
                tokens.append(Token("OP", matched_op, line_no, column))
                i += len(matched_op)
                continue

            if char.isdigit():
                match = self._number_pattern.match(content, i)
                assert match is not None
                lexeme = match.group(0)
                value = float(lexeme) if any(symbol in lexeme for symbol in ".eE") else int(lexeme)
                tokens.append(Token("NUMBER", value, line_no, column))
                i = match.end()
                continue

            if char.isalpha() or char == "_":
                match = self._name_pattern.match(content, i)
                assert match is not None
                name = match.group(0)
                if name in self._keywords:
                    kind = "BOOL" if name in {"True", "False"} else "KEYWORD"
                    value = (name == "True") if kind == "BOOL" else name
                    tokens.append(Token(kind, value, line_no, column))
                else:
                    tokens.append(Token("NAME", name, line_no, column))
                i = match.end()
                continue

            token_kind = self._single_char_tokens.get(char)
            if token_kind is not None:
                tokens.append(Token(token_kind, char, line_no, column))
                i += 1
                continue

            raise MiniPythonSyntaxError(
                f"Рядок {line_no}, позиція {column}: недопустимий символ {char!r}."
            )

        return tokens


class Parser:
    """Рекурсивний спуск за мінімізованою граматикою."""

    def __init__(self, tokens: Iterable[Token]):
        self.tokens = list(tokens)
        self.position = 0

    def parse(self) -> Program:
        statements = self._parse_block(stop_kinds={"EOF"})
        self._expect("EOF")
        return Program(statements)

    def _current(self) -> Token:
        return self.tokens[self.position]

    def _peek(self, offset: int = 1) -> Token:
        index = min(self.position + offset, len(self.tokens) - 1)
        return self.tokens[index]

    def _advance(self) -> Token:
        token = self._current()
        if self.position < len(self.tokens) - 1:
            self.position += 1
        return token

    def _expect(self, kind: str, value: Any | None = None) -> Token:
        token = self._current()
        if token.kind != kind or (value is not None and token.value != value):
            expected = f"{kind} {value!r}" if value is not None else kind
            raise MiniPythonSyntaxError(
                f"Рядок {token.line}, позиція {token.column}: очікувалось {expected}, "
                f"отримано {token.kind} {token.value!r}."
            )
        self._advance()
        return token

    def _match(self, kind: str, value: Any | None = None) -> bool:
        token = self._current()
        if token.kind == kind and (value is None or token.value == value):
            self._advance()
            return True
        return False

    def _parse_block(self, stop_kinds: set[str]) -> list[Any]:
        statements: list[Any] = []
        while self._current().kind not in stop_kinds:
            if self._match("NEWLINE"):
                continue
            statements.append(self._parse_statement())
        return statements

    def _parse_statement(self) -> Any:
        token = self._current()

        if token.kind == "KEYWORD" and token.value == "print":
            statement = self._parse_print()
        elif token.kind == "KEYWORD" and token.value == "if":
            statement = self._parse_if()
        elif token.kind == "KEYWORD" and token.value == "while":
            statement = self._parse_while()
        elif token.kind == "NAME" and self._peek().kind == "ASSIGN":
            statement = self._parse_assignment()
        else:
            raise MiniPythonSyntaxError(
                f"Рядок {token.line}, позиція {token.column}: невідомий початок оператора "
                f"{token.value!r}."
            )

        if self._current().kind == "NEWLINE":
            self._advance()
        return statement

    def _parse_assignment(self) -> AssignStmt:
        name_token = self._expect("NAME")
        self._expect("ASSIGN")
        expression = self._parse_expression()
        return AssignStmt(name_token.value, expression)

    def _parse_print(self) -> PrintStmt:
        self._expect("KEYWORD", "print")
        self._expect("LPAREN")
        expressions: list[Any] = []
        if self._current().kind != "RPAREN":
            expressions.append(self._parse_expression())
            while self._match("COMMA"):
                expressions.append(self._parse_expression())
        self._expect("RPAREN")
        return PrintStmt(expressions)

    def _parse_if(self) -> IfStmt:
        self._expect("KEYWORD", "if")
        condition = self._parse_expression()
        self._expect("COLON")
        then_block = self._parse_suite()

        else_block = None
        if self._current().kind == "KEYWORD" and self._current().value == "else":
            self._advance()
            self._expect("COLON")
            else_block = self._parse_suite()

        return IfStmt(condition, then_block, else_block)

    def _parse_while(self) -> WhileStmt:
        self._expect("KEYWORD", "while")
        condition = self._parse_expression()
        self._expect("COLON")
        body = self._parse_suite()
        return WhileStmt(condition, body)

    def _parse_suite(self) -> list[Any]:
        self._expect("NEWLINE")
        self._expect("INDENT")
        statements = self._parse_block(stop_kinds={"DEDENT", "EOF"})
        self._expect("DEDENT")
        return statements

    def _parse_expression(self) -> Any:
        return self._parse_or()

    def _parse_or(self) -> Any:
        left = self._parse_and()
        while self._current().kind == "KEYWORD" and self._current().value == "or":
            operator = self._advance().value
            right = self._parse_and()
            left = BinaryOp(left, operator, right)
        return left

    def _parse_and(self) -> Any:
        left = self._parse_not()
        while self._current().kind == "KEYWORD" and self._current().value == "and":
            operator = self._advance().value
            right = self._parse_not()
            left = BinaryOp(left, operator, right)
        return left

    def _parse_not(self) -> Any:
        if self._current().kind == "KEYWORD" and self._current().value == "not":
            operator = self._advance().value
            return UnaryOp(operator, self._parse_not())
        return self._parse_comparison()

    def _parse_comparison(self) -> Any:
        left = self._parse_arith_expr()
        while self._current().kind == "OP" and self._current().value in {"==", "!=", "<", "<=", ">", ">="}:
            operator = self._advance().value
            right = self._parse_arith_expr()
            left = BinaryOp(left, operator, right)
        return left

    def _parse_arith_expr(self) -> Any:
        left = self._parse_term()
        while self._current().kind == "OP" and self._current().value in {"+", "-"}:
            operator = self._advance().value
            right = self._parse_term()
            left = BinaryOp(left, operator, right)
        return left

    def _parse_term(self) -> Any:
        left = self._parse_unary()
        while self._current().kind == "OP" and self._current().value in {"*", "/", "//", "%"}:
            operator = self._advance().value
            right = self._parse_unary()
            left = BinaryOp(left, operator, right)
        return left

    def _parse_unary(self) -> Any:
        if self._current().kind == "OP" and self._current().value in {"+", "-"}:
            operator = self._advance().value
            return UnaryOp(operator, self._parse_unary())
        return self._parse_power()

    def _parse_power(self) -> Any:
        left = self._parse_atom()
        if self._current().kind == "OP" and self._current().value == "**":
            operator = self._advance().value
            right = self._parse_unary()
            return BinaryOp(left, operator, right)
        return left

    def _parse_atom(self) -> Any:
        token = self._current()

        if token.kind == "NUMBER":
            self._advance()
            return Number(token.value)

        if token.kind == "BOOL":
            self._advance()
            return Boolean(token.value)

        if token.kind == "NAME":
            if self._peek().kind == "LPAREN":
                return self._parse_call()
            self._advance()
            return Variable(token.value)

        if token.kind == "LPAREN":
            self._advance()
            expression = self._parse_expression()
            self._expect("RPAREN")
            return expression

        raise MiniPythonSyntaxError(
            f"Рядок {token.line}, позиція {token.column}: очікувалось число, змінна, "
            f"функція або дужки, отримано {token.kind} {token.value!r}."
        )

    def _parse_call(self) -> Call:
        name = self._expect("NAME").value
        self._expect("LPAREN")
        arguments: list[Any] = []
        if self._current().kind != "RPAREN":
            arguments.append(self._parse_expression())
            while self._match("COMMA"):
                arguments.append(self._parse_expression())
        self._expect("RPAREN")
        return Call(name, arguments)


class Executor:
    """Виконує синтаксичне дерево програми у словнику змінних."""

    def __init__(self, input_stream: TextIO | None = None, output_stream: TextIO | None = None):
        self.variables: dict[str, Any] = {}
        self.input_stream = input_stream if input_stream is not None else sys.stdin
        self.output_stream = output_stream if output_stream is not None else sys.stdout
        self.functions = {
            "sin": self._fn_sin,
            "cos": self._fn_cos,
            "int": self._fn_int,
            "float": self._fn_float,
            "input": self._fn_input,
        }

    def execute(self, program: Program) -> None:
        for statement in program.statements:
            self._execute_statement(statement)

    def _execute_statement(self, statement: Any) -> None:
        if isinstance(statement, AssignStmt):
            self.variables[statement.name] = self._evaluate(statement.expression)
            return

        if isinstance(statement, PrintStmt):
            values = [self._evaluate(expr) for expr in statement.expressions]
            rendered = " ".join(self._format_value(value) for value in values)
            self.output_stream.write(rendered + "\n")
            return

        if isinstance(statement, IfStmt):
            block = statement.then_block if self._truthy(self._evaluate(statement.condition)) else statement.else_block
            if block is not None:
                for nested in block:
                    self._execute_statement(nested)
            return

        if isinstance(statement, WhileStmt):
            while self._truthy(self._evaluate(statement.condition)):
                for nested in statement.body:
                    self._execute_statement(nested)
            return

        raise MiniPythonRuntimeError(f"Невідомий тип оператора: {statement!r}.")

    def _evaluate(self, node: Any) -> Any:
        if isinstance(node, Number):
            return node.value

        if isinstance(node, Boolean):
            return node.value

        if isinstance(node, Variable):
            if node.name not in self.variables:
                raise MiniPythonRuntimeError(f"Змінна {node.name!r} використана до присвоєння.")
            return self.variables[node.name]

        if isinstance(node, UnaryOp):
            value = self._evaluate(node.operand)
            if node.operator == "+":
                return +value
            if node.operator == "-":
                return -value
            if node.operator == "not":
                return not self._truthy(value)
            raise MiniPythonRuntimeError(f"Непідтримувана унарна операція {node.operator!r}.")

        if isinstance(node, BinaryOp):
            if node.operator == "and":
                return self._truthy(self._evaluate(node.left)) and self._truthy(self._evaluate(node.right))
            if node.operator == "or":
                return self._truthy(self._evaluate(node.left)) or self._truthy(self._evaluate(node.right))

            left = self._evaluate(node.left)
            right = self._evaluate(node.right)

            operations = {
                "+": lambda a, b: a + b,
                "-": lambda a, b: a - b,
                "*": lambda a, b: a * b,
                "/": lambda a, b: a / b,
                "//": lambda a, b: a // b,
                "%": lambda a, b: a % b,
                "**": lambda a, b: a**b,
                "==": lambda a, b: a == b,
                "!=": lambda a, b: a != b,
                "<": lambda a, b: a < b,
                "<=": lambda a, b: a <= b,
                ">": lambda a, b: a > b,
                ">=": lambda a, b: a >= b,
            }

            try:
                return operations[node.operator](left, right)
            except ZeroDivisionError as error:
                raise MiniPythonRuntimeError("Ділення на нуль.") from error
            except OverflowError as error:
                raise MiniPythonRuntimeError("Переповнення під час обчислення.") from error
            except KeyError as error:
                raise MiniPythonRuntimeError(
                    f"Непідтримувана бінарна операція {node.operator!r}."
                ) from error

        if isinstance(node, Call):
            if node.name not in self.functions:
                raise MiniPythonRuntimeError(f"Невідома функція {node.name!r}.")
            arguments = [self._evaluate(arg) for arg in node.arguments]
            return self.functions[node.name](arguments)

        raise MiniPythonRuntimeError(f"Невідомий тип виразу: {node!r}.")

    def _truthy(self, value: Any) -> bool:
        return bool(value)

    def _format_value(self, value: Any) -> str:
        if isinstance(value, bool):
            return "True" if value else "False"
        return str(value)

    def _fn_sin(self, arguments: list[Any]) -> float:
        self._require_arity("sin", arguments, 1)
        return math.sin(arguments[0])

    def _fn_cos(self, arguments: list[Any]) -> float:
        self._require_arity("cos", arguments, 1)
        return math.cos(arguments[0])

    def _fn_int(self, arguments: list[Any]) -> int:
        self._require_arity("int", arguments, 1)
        return int(arguments[0])

    def _fn_float(self, arguments: list[Any]) -> float:
        self._require_arity("float", arguments, 1)
        return float(arguments[0])

    def _fn_input(self, arguments: list[Any]) -> Any:
        self._require_arity("input", arguments, 0)
        line = self.input_stream.readline()
        if line == "":
            raise MiniPythonRuntimeError("Очікувався рядок для input(), але вхідний потік завершився.")
        return self._convert_input(line.strip())

    def _convert_input(self, text: str) -> Any:
        lowered = text.lower()
        if lowered == "true":
            return True
        if lowered == "false":
            return False

        if re.fullmatch(r"[+-]?\d+", text):
            return int(text)

        if re.fullmatch(r"[+-]?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?", text):
            return float(text)

        raise MiniPythonRuntimeError(
            f"input() не зміг автоматично перетворити значення {text!r} до числового або логічного типу."
        )

    def _require_arity(self, name: str, arguments: list[Any], expected: int) -> None:
        if len(arguments) != expected:
            raise MiniPythonRuntimeError(
                f"Функція {name}() очікує {expected} арг., отримано {len(arguments)}."
            )


def interpret_source(source: str, input_stream: TextIO | None = None, output_stream: TextIO | None = None) -> None:
    tokens = Lexer(source).tokenize()
    program = Parser(tokens).parse()
    Executor(input_stream=input_stream, output_stream=output_stream).execute(program)


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        print(
            "Використання: python mini_python_interpreter.py <шлях_до_програми>",
            file=sys.stderr,
        )
        return 1

    source_path = pathlib.Path(argv[1])

    try:
        source = source_path.read_text(encoding="utf-8")
    except OSError as error:
        print(f"Не вдалося прочитати файл {source_path}: {error}", file=sys.stderr)
        return 1

    try:
        interpret_source(source)
    except MiniPythonError as error:
        print(f"Помилка інтерпретації: {error}", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
