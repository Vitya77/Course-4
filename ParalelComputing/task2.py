import time
import numpy as np
from numba import njit, prange

n1 = 100000
n2 = 100000

x0 = np.ones(n2 + 2, dtype=np.float64)

@njit
def sequential(x):
    x = x.copy()
    for j in range(1, n1 + 1):
        for i in range(1, n2 + 1):
            x[i] = x[i - 1] + x[i] + x[i + 1]
    return x

@njit(parallel=True)
def parallel_func(x):
    x = x.copy()

    max_v = 2 * n1 + n2 - 2

    for v in range(1, max_v + 1):

        valid_i = []
        for i in range(1, n2 + 1):
            j = 0.5 * (-i + v + 2)
            if 1 <= j <= n1 and j == int(j):
                valid_i.append(i)

        vi = np.array(valid_i)

        for k in prange(len(vi)):
            i = vi[k]
            x[i] = x[i - 1] + x[i] + x[i + 1]

    return x


if __name__ == "__main__":
    print("Компіляція послідовного...")
    sequential(x0)

    print("Компіляція паралельного...")
    parallel_func(x0)

    print("\nЗапуск послідовного...")
    t0 = time.time()
    seq_result = sequential(x0)
    t1 = time.time()
    T_seq = t1 - t0
    print(f"Послідовний час: {T_seq:.4f} секунд")

    print("\nЗапуск паралельного...")
    t0 = time.time()
    par_result = parallel_func(x0)
    t1 = time.time()
    T_par = t1 - t0
    print(f"Паралельний час: {T_par:.4f} секунд")

    speedup = T_seq / T_par
    print(f"\nПрискорення: {speedup:.2f}x")
