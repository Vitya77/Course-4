import random
from math import gcd

def is_prime(num):
    """Перевірка простоти числа (детермінована для невеликих чисел)."""
    if num < 2:
        return False
    if num % 2 == 0:
        return num == 2
    for i in range(3, int(num ** 0.5) + 1, 2):
        if num % i == 0:
            return False
    return True


def generate_prime(start=1000, end=5000):
    """Генерує випадкове просте число у заданому діапазоні, яке ≡ 3 mod 4."""
    while True:
        p = random.randint(start, end)
        if is_prime(p) and p % 4 == 3:
            return p

class BBSGenerator:
    def __init__(self, p, q, seed):
        self.n = p * q
        self.state = seed % self.n

    def next_bit(self):
        """Генерує наступний біт."""
        self.state = pow(self.state, 2, self.n)
        return self.state % 2

    def next_byte(self):
        """Генерує байт (8 бітів)."""
        byte = 0
        for _ in range(8):
            byte = (byte << 1) | self.next_bit()
        return byte

def bbs_encrypt_decrypt(data, bbs):
    if isinstance(data, str):
        data = data.encode('utf-8')
    return bytes([b ^ bbs.next_byte() for b in data])


def main():
    p = generate_prime(1000, 5000)
    q = generate_prime(1000, 5000)
    n = p * q
    print(f"p = {p}, q = {q}, n = {n}")

    seed = random.randint(2, n - 1)
    while gcd(seed, n) != 1:
        seed = random.randint(2, n - 1)

    print(f"seed = {seed}")

    bbs_enc = BBSGenerator(p, q, seed)

    message = "Hello, its testing text!"
    print(f"\nВихідне повідомлення: {message}")

    encrypted = bbs_encrypt_decrypt(message, bbs_enc)
    print(f"Зашифрований текст (у байтах): {encrypted}")

    bbs_dec = BBSGenerator(p, q, seed)
    decrypted = bbs_encrypt_decrypt(encrypted, bbs_dec).decode('utf-8')
    print(f"Розшифроване повідомлення: {decrypted}")


if __name__ == "__main__":
    main()
