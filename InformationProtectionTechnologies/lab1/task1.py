import string

def prepare_key_matrix(key):
    key = key.upper().replace('J', 'I')
    matrix = []
    used = set()
    for char in key:
        if char in string.ascii_uppercase and char not in used:
            matrix.append(char)
            used.add(char)
    for char in string.ascii_uppercase:
        if char == 'J':
            continue
        if char not in used:
            matrix.append(char)
            used.add(char)
    return [matrix[i*5:(i+1)*5] for i in range(5)]

def find_position(matrix, char):
    for i, row in enumerate(matrix):
        for j, c in enumerate(row):
            if c == char:
                return i, j
    return None

def prepare_text(text, for_encrypt=True):
    text = text.upper().replace('J', 'I')
    text = ''.join([c for c in text if c in string.ascii_uppercase])
    result = ''
    i = 0
    while i < len(text):
        a = text[i]
        b = text[i+1] if i+1 < len(text) else 'X'
        if a == b:
            result += a + 'X'
            i += 1
        else:
            result += a + b
            i += 2
    if len(result) % 2 != 0:
        result += 'X'
    return result

def playfair_encrypt(plain_text, matrix):
    text = prepare_text(plain_text)
    cipher = ''
    for i in range(0, len(text), 2):
        a, b = text[i], text[i+1]
        row_a, col_a = find_position(matrix, a)
        row_b, col_b = find_position(matrix, b)
        if row_a == row_b:
            cipher += matrix[row_a][(col_a+1)%5]
            cipher += matrix[row_b][(col_b+1)%5]
        elif col_a == col_b:
            cipher += matrix[(row_a+1)%5][col_a]
            cipher += matrix[(row_b+1)%5][col_b]
        else:
            cipher += matrix[row_a][col_b]
            cipher += matrix[row_b][col_a]
    return cipher

def playfair_decrypt(cipher_text, matrix):
    text = prepare_text(cipher_text, for_encrypt=False)
    plain = ''
    for i in range(0, len(text), 2):
        a, b = text[i], text[i+1]
        row_a, col_a = find_position(matrix, a)
        row_b, col_b = find_position(matrix, b)
        if row_a == row_b:
            plain += matrix[row_a][(col_a-1)%5]
            plain += matrix[row_b][(col_b-1)%5]
        elif col_a == col_b:
            plain += matrix[(row_a-1)%5][col_a]
            plain += matrix[(row_b-1)%5][col_b]
        else:
            plain += matrix[row_a][col_b]
            plain += matrix[row_b][col_a]
    return plain

def main():
    print("Оберіть режим:")
    print("1 - Шифрування")
    print("2 - Розшифрування")
    mode = input("Введіть 1 або 2: ").strip()
    key = input("Введіть ключове слово: ").strip()
    matrix = prepare_key_matrix(key)
    if mode == '1':
        input_file = input("Введіть ім'я файлу з текстом для шифрування: ").strip()
        output_file = input("Введіть ім'я файлу для збереження шифротексту: ").strip()
        with open(input_file, 'r', encoding='utf-8') as f:
            plain_text = f.read()
        cipher_text = playfair_encrypt(plain_text, matrix)
        with open(output_file, 'w', encoding='utf-8') as f:
            f.write(cipher_text)
        print("Зашифрований текст записано у файл", output_file)
    elif mode == '2':
        input_file = input("Введіть ім'я файлу із зашифрованим текстом: ").strip()
        with open(input_file, 'r', encoding='utf-8') as f:
            cipher_text = f.read()
        plain_text = playfair_decrypt(cipher_text, matrix)
        print("Розшифрований текст:")
        print(plain_text)
    else:
        print("Невірний режим!")

if __name__ == "__main__":
    main()