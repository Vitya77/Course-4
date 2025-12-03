import hashlib
import json
import secrets
import math
from typing import Tuple, List

# ------------------ Utility functions ------------------

def is_probable_prime(n: int, k: int = 8) -> bool:
    """Miller-Rabin primality test (probabilistic)."""
    if n < 2:
        return False
    small_primes = [2,3,5,7,11,13,17,19,23,29]
    for p in small_primes:
        if n % p == 0:
            return n == p
    # write n-1 as 2^s * d
    d = n - 1
    s = 0
    while d % 2 == 0:
        d //= 2
        s += 1
    for _ in range(k):
        a = secrets.randbelow(n - 3) + 2  # in [2, n-2]
        x = pow(a, d, n)
        if x == 1 or x == n - 1:
            continue
        skip_to_next_n = False
        for _ in range(s - 1):
            x = (x * x) % n
            if x == n - 1:
                skip_to_next_n = True
                break
        if skip_to_next_n:
            continue
        return False
    return True

def gen_random_odd(bits: int) -> int:
    n = secrets.randbits(bits)
    n |= (1 << (bits - 1)) | 1
    return n

def gen_prime(bits: int) -> int:
    while True:
        candidate = gen_random_odd(bits)
        if is_probable_prime(candidate):
            return candidate

def gen_safe_prime(bits: int) -> Tuple[int,int]:
    """
    Generate a safe prime p = 2*q + 1 where q is prime.
    Returns (p, q)
    """
    if bits < 3:
        raise ValueError("bits must be >= 3")
    q_bits = bits - 1
    while True:
        q = gen_prime(q_bits)
        p = 2 * q + 1
        if is_probable_prime(p):
            return p, q

def modinv(a: int, m: int) -> int:
    """Modular inverse by extended Euclid."""
    g, x, y = extended_gcd(a, m)
    if g != 1:
        raise ValueError("No modular inverse")
    return x % m

def extended_gcd(a: int, b: int) -> Tuple[int,int,int]:
    if b == 0:
        return a, 1, 0
    g, x1, y1 = extended_gcd(b, a % b)
    return g, y1, x1 - (a // b) * y1

# ------------------ ElGamal ------------------

def find_generator_safe(p: int, q: int) -> int:
    """
    For safe prime p = 2*q + 1, find a primitive root g.
    Conditions: g^2 != 1 (mod p) and g^q != 1 (mod p)
    """
    for _ in range(1000):
        g = secrets.randbelow(p - 3) + 2
        if pow(g, 2, p) != 1 and pow(g, q, p) != 1:
            return g
    raise RuntimeError("Failed to find generator")

def elgamal_keygen(p_bits: int = 1024):
    p, q = gen_safe_prime(p_bits)
    g = find_generator_safe(p, q)
    a = secrets.randbelow(p - 2) + 1  # secret key 1..p-2
    h = pow(g, a, p)
    return {
        "p": p,
        "g": g,
        "a": a,   # secret
        "h": h
    }

def elgamal_encrypt_bytes(msg_bytes: bytes, p: int, g: int, h: int) -> List[Tuple[str,str,int]]:
    """
    Encrypt message bytes in chunks (each chunk as integer < p).
    Returns list of pairs (c1_hex, c2_hex, chunk_len)
    """
    max_chunk_bytes = (p.bit_length() - 1) // 8
    if max_chunk_bytes <= 0:
        raise ValueError("p too small to encode bytes")
    pairs = []
    for i in range(0, len(msg_bytes), max_chunk_bytes):
        chunk = msg_bytes[i:i+max_chunk_bytes]
        m = int.from_bytes(chunk, byteorder="big")
        if m >= p:
            raise ValueError("chunk integer >= p, increase p or reduce chunk size")
        k = secrets.randbelow(p - 2) + 1
        c1 = pow(g, k, p)
        s = pow(h, k, p)
        c2 = (m * s) % p
        pairs.append((hex(c1)[2:], hex(c2)[2:], len(chunk)))
    return pairs

def elgamal_decrypt_pairs(pairs: List[Tuple[str,str,int]], p: int, a: int) -> bytes:
    out = bytearray()
    for c1_hex, c2_hex, chunk_len in pairs:
        c1 = int(c1_hex, 16)
        c2 = int(c2_hex, 16)
        s = pow(c1, a, p)
        s_inv = modinv(s, p)
        m = (c2 * s_inv) % p
        chunk = m.to_bytes(chunk_len, byteorder="big")
        out.extend(chunk)
    return bytes(out)

# ------------------ RSA for signature ------------------

def rsa_keygen(bits: int = 2048, e: int = 65537) -> Tuple[int,int,int]:
    while True:
        p = gen_prime(bits // 2)
        q = gen_prime(bits // 2)
        if p == q:
            continue
        n = p * q
        phi = (p - 1) * (q - 1)
        if math.gcd(e, phi) == 1:
            d = modinv(e, phi)
            return n, e, d

def rsa_sign(hash_bytes: bytes, d: int, n: int) -> int:
    h_int = int.from_bytes(hash_bytes, byteorder="big")
    sig = pow(h_int, d, n)
    return sig

def rsa_verify(hash_bytes: bytes, sig_int: int, e: int, n: int) -> bool:
    h_int = int.from_bytes(hash_bytes, byteorder="big")
    recovered = pow(sig_int, e, n)
    return recovered == h_int

# ------------------ File helpers ------------------

def read_plaintext_file(path: str = "plaintext.txt") -> bytes:
    with open(path, "rb") as f:
        return f.read()

def write_decrypted_file(data: bytes, path: str = "decrypted.txt"):
    with open(path, "wb") as f:
        f.write(data)

# ------------------ Main flow ------------------

def main():
    print("Reading plaintext from plaintext.txt ...")
    plaintext = read_plaintext_file("plaintext.txt")
    print(f"Plaintext length: {len(plaintext)} bytes")

    # 1) ElGamal keygen
    print("Generating ElGamal keys (safe prime p). This may take a while...")
    elg = elgamal_keygen(p_bits=512)  # change bits if needed
    p = elg["p"]
    g = elg["g"]
    a = elg["a"]
    h = elg["h"]
    print(f"ElGamal: p (bits)={p.bit_length()}, g={g}, h={h}")

    # 2) Encrypt plaintext
    print("Encrypting plaintext with ElGamal...")
    pairs = elgamal_encrypt_bytes(plaintext, p, g, h)
    # Save ciphertext + public params
    ciphertext_obj = {
        "p": hex(p)[2:],
        "g": hex(g)[2:],
        "h": hex(h)[2:],
        "pairs": [
            {"c1": c1, "c2": c2, "len": clen}
            for (c1, c2, clen) in pairs
        ]
    }
    with open("ciphertext.json", "w", encoding="utf-8") as f:
        json.dump(ciphertext_obj, f, indent=2)
    print("Ciphertext written to ciphertext.json")

    # 3) RSA keygen and signature (sign the plaintext hash)
    print("Generating RSA keypair for signature...")
    n, e, d = rsa_keygen(bits=2048)
    hash_bytes = hashlib.sha256(plaintext).digest()
    sig_int = rsa_sign(hash_bytes, d, n)
    signature_obj = {
        "n": hex(n)[2:],
        "e": e,
        "signature": hex(sig_int)[2:]
    }
    with open("signature.json", "w", encoding="utf-8") as f:
        json.dump(signature_obj, f, indent=2)
    print("Signature and RSA public key written to signature.json")

    # 4) Verification flow: read ciphertext and signature, decrypt and verify
    print("\n--- Verification step ---")
    print("Reading ciphertext.json and signature.json...")
    with open("ciphertext.json", "r", encoding="utf-8") as f:
        ct = json.load(f)
    with open("signature.json", "r", encoding="utf-8") as f:
        sigfile = json.load(f)

    p_read = int(ct["p"], 16)
    g_read = int(ct["g"], 16)
    h_read = int(ct["h"], 16)
    pairs_read = [(item["c1"], item["c2"], item["len"]) for item in ct["pairs"]]

    # decrypt using secret 'a' we generated above
    print("Decrypting ciphertext with ElGamal secret key...")
    decrypted = elgamal_decrypt_pairs(pairs_read, p_read, a)
    write_decrypted_file(decrypted, "decrypted.txt")
    print("Decrypted message written to decrypted.txt")

    # compute hash and verify RSA signature
    print("Computing SHA-256 of decrypted message and verifying RSA signature...")
    decrypted_hash = hashlib.sha256(decrypted).digest()
    n_read = int(sigfile["n"], 16)
    e_read = int(sigfile["e"])
    sig_int_read = int(sigfile["signature"], 16)

    verified = rsa_verify(decrypted_hash, sig_int_read, e_read, n_read)
    if verified:
        print("✅ Signature verified: message and signature ARE authentic.")
    else:
        print("❌ Signature verification FAILED: message or signature NOT authentic.")

if __name__ == "__main__":
    main()
