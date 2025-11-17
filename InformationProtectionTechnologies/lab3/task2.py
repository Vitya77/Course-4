def lfsr_generate(n=8, taps=[7, 5, 1, 0], seed="10101100", steps=20):
    """
    Моделює роботу n-розрядного регістра зсуву зі зворотним зв'язком.
    :param n: кількість розрядів (довжина регістра)
    :param taps: позиції бітів, які беруть участь у зворотному зв’язку (0 — правий біт)
    :param seed: початковий стан регістра у вигляді рядка бітів
    :param steps: кількість кроків для виводу
    """
    state = [int(b) for b in seed]
    print(f"{'Номер стану':<12}{'Внутрішній стан':<20}{'f (зворотний зв’язок)':<25}{'Біт, який витягується (b1)':<30}")
    print("-" * 85)

    for i in range(steps):
        out_bit = state[-1]  # витягується правий біт
        f = 0
        for t in taps:
            f ^= state[-(t + 1)]

        state = [f] + state[:-1]
        print(f"{i:<12}{''.join(map(str, state)):<20}{f:<25}{out_bit:<30}")

    print("-" * 85)


def xor_bits(a, b):
    """Побітове XOR для двох двійкових рядків."""
    return ''.join(str(int(x) ^ int(y)) for x, y in zip(a, b))


def encrypt_decrypt(message, key):
    """Шифрування та розшифрування тексту з алфавіту Z2 (0/1)."""
    key = (key * ((len(message) // len(key)) + 1))[:len(message)]
    return xor_bits(message, key)

if __name__ == "__main__":
    taps = [7, 5, 1, 0]  # x^8 + x^6 + x^2 + 1
    seed = "10101100"

    lfsr_generate(8, taps, seed, steps=15)

    plaintext = "1101011001110001"
    key = "1010100110010110"
    ciphertext = encrypt_decrypt(plaintext, key)
    decrypted = encrypt_decrypt(ciphertext, key)

    print("\nВідкритий текст:     ", plaintext)
    print("Ключ:                ", key)
    print("Зашифрований текст:  ", ciphertext)
    print("Розшифрований текст: ", decrypted)
