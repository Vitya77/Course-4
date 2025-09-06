import numpy as np

# 1. Визначаємо алфавіт
ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ '
MOD = len(ALPHABET)

def text_to_numbers(text):
    # Перетворює текст у список чисел
    return [ALPHABET.index(c) for c in text.upper() if c in ALPHABET]

def numbers_to_text(numbers):
    # Перетворює список чисел у текст
    return ''.join(ALPHABET[n % MOD] for n in numbers)

def keyword_to_matrix(keyword):
    # Генерує квадратну матрицю з ключового слова
    keyword = keyword.upper()
    size = int(len(keyword) ** 0.5)
    if size * size != len(keyword):
        raise ValueError("Довжина ключового слова має бути квадратом цілого числа (наприклад, 9, 16, 25...)")
    nums = text_to_numbers(keyword)
    matrix = np.array(nums).reshape((size, size))
    det = int(round(np.linalg.det(matrix)))
    if det == 0 or np.gcd(det, MOD) != 1:
        raise ValueError("Матриця не підходить: детермінант має бути ненульовим і взаємно простим з модулем.")
    return matrix

def mod_inv(a, m):
    # Обернений елемент по модулю m
    for i in range(1, m):
        if (a * i) % m == 1:
            return i
    raise ValueError("Обернений елемент не існує")

def matrix_mod_inv(matrix, mod):
    # Обернена матриця по модулю mod
    det = int(round(np.linalg.det(matrix)))
    det_inv = mod_inv(det % mod, mod)
    matrix_adj = np.round(det * np.linalg.inv(matrix)).astype(int) % mod
    inv_matrix = (det_inv * matrix_adj) % mod
    return inv_matrix

def pad_text(text, size):
    # Додає пробіли до тексту, щоб його довжина ділилась на size
    while len(text) % size != 0:
        text += ' '
    return text

def encrypt(text, matrix):
    size = matrix.shape[0]
    text = pad_text(text, size)
    nums = text_to_numbers(text)
    cipher_nums = []
    for i in range(0, len(nums), size):
        block = np.array(nums[i:i+size])
        cipher_block = np.dot(matrix, block) % MOD
        cipher_nums.extend(cipher_block)
    return numbers_to_text(cipher_nums)

def decrypt(cipher_text, matrix):
    size = matrix.shape[0]
    nums = text_to_numbers(cipher_text)
    inv_matrix = matrix_mod_inv(matrix, MOD)
    plain_nums = []
    for i in range(0, len(nums), size):
        block = np.array(nums[i:i+size])
        plain_block = np.dot(inv_matrix, block) % MOD
        plain_nums.extend(plain_block)
    return numbers_to_text(plain_nums)

def read_file(path):
    with open(path, 'r', encoding='utf-8') as f:
        return f.read()

def write_file(path, text):
    with open(path, 'w', encoding='utf-8') as f:
        f.write(text)

def main():
    print("Виберіть режим:")
    print("1 - Шифрування")
    print("2 - Дешифрування")
    mode = input("Ваш вибір: ")
    if mode == '1':
        in_file = input("Введіть шлях до файлу з відкритим текстом: ")
        text = read_file(in_file)
        keyword = input("Введіть ключове слово (довжина: 9, 16, 25...): ")
        matrix = keyword_to_matrix(keyword)
        print("Ключ-матриця:")
        print(matrix)
        cipher_text = encrypt(text, matrix)
        out_file = input("Введіть шлях для запису шифротексту: ")
        write_file(out_file, cipher_text)
        print("Шифротекст записано у файл.")
    elif mode == '2':
        in_file = input("Введіть шлях до файлу з шифротекстом: ")
        cipher_text = read_file(in_file)
        keyword = input("Введіть ключове слово: ")
        matrix = keyword_to_matrix(keyword)
        print("Ключ-матриця:")
        print(matrix)
        plain_text = decrypt(cipher_text, matrix)
        out_file = input("Введіть шлях для запису розшифрованого тексту: ")
        write_file(out_file, plain_text)
        print("Розшифрований текст:")
        print(plain_text)
    else:
        print("Невірний вибір.")

if __name__ == "__main__":
    main()
