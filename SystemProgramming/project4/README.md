# Project 4: Міні-інтерпретатор ~Python

У цій папці реалізовано факультативну частину 4 лабораторної роботи 12 з дисципліни "ОС і СП".

## Склад файлів

- `mini_python_interpreter.py` — повний код інтерпретатора з коментарями.
- `grammar_part4.txt` — остаточний варіант граматики реалізованої мови.
- `tests/` — три тестові приклади програм.
- `results/` — результати інтерпретації тестових прикладів.

## Підтримувані елементи

- типи: `int`, `float`, `bool`;
- арифметика: `+`, `-`, `*`, `/`, `//`, `%`, `**`, унарні `+` і `-`;
- логіка: `and`, `or`, `not`, `==`, `!=`, `<`, `<=`, `>`, `>=`;
- функції: `sin()`, `cos()`, `int()`, `float()`, `input()`;
- оператори: присвоєння, `print()`, `if-else`, `while`;
- блоки операторів з відступами пробілами.

## Запуск

```bash
python3 mini_python_interpreter.py tests/test1_arithmetic_if.mpy
python3 mini_python_interpreter.py tests/test2_input_while.mpy < tests/test2_input.txt
python3 mini_python_interpreter.py tests/test3_logic_nested.mpy
```
