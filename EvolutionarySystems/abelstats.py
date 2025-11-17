import numpy as np
import matplotlib.pyplot as plt
import random
from scipy.stats import linregress

# --- Параметри ---
N = 25
num_drops = 50000  # кількість піщинок
sandpile = np.zeros((N, N), dtype=int)
center = (N // 2, N // 2)
avalanche_sizes = []

# --- Функція "обвалу" ---
def topple(grid):
    unstable = True
    topple_count = 0
    while unstable:
        unstable = False
        to_topple = grid >= 6
        if np.any(to_topple):
            unstable = True
            topple_count += np.sum(to_topple)
            grid[to_topple] -= 6
            for i, j in zip(*np.where(to_topple)):
                if i > 0: grid[i - 1, j] += 1
                if i < N - 1: grid[i + 1, j] += 1
                if j > 0: grid[i, j - 1] += 1
                if j < N - 1: grid[i, j + 1] += 1
    return topple_count

# --- Основний цикл ---
for step in range(num_drops):
    sandpile[center] += 1
    avalanche_size = topple(sandpile)
    if avalanche_size > 0:
        avalanche_sizes.append(avalanche_size)
    if (step + 1) % (num_drops // 10) == 0:
        print(f"Прогрес: {(step + 1) / num_drops * 100:.0f}%")

# --- Побудова зрозуміліших графіків ---
if not avalanche_sizes:
    print("Недостатньо даних для аналізу.")
    exit()

# 1️⃣ Звичайна гістограма
plt.figure(figsize=(8,5))
plt.hist(avalanche_sizes, bins=50, color='steelblue', edgecolor='black')
plt.xlabel("Розмір лавини (кількість обвалених клітин)")
plt.ylabel("Кількість випадків")
plt.title("Розподіл розмірів лавин у моделі Авелевої купи")
plt.grid(alpha=0.4)
plt.show()

# 2️⃣ Кумулятивний розподіл (ймовірність, що лавина ≥ s)
sorted_sizes = np.sort(avalanche_sizes)
cum_prob = 1.0 - np.arange(len(sorted_sizes)) / len(sorted_sizes)
plt.figure(figsize=(8,5))
plt.plot(sorted_sizes, cum_prob, color='orange')
plt.xscale('log')
plt.yscale('log')
plt.xlabel("Розмір лавини s")
plt.ylabel("P(лавина ≥ s)")
plt.title("Кумулятивний розподіл")
plt.grid(True, which="both", ls="--", alpha=0.5)
plt.show()

# 3️⃣ Лог-лог з апроксимацією (науковий графік)
counts, bins = np.histogram(avalanche_sizes, bins=np.logspace(0, np.log10(max(avalanche_sizes)+1), 50))
bin_centers = 0.5 * (bins[1:] + bins[:-1])
prob = counts / np.sum(counts)

mask = prob > 0
log_s = np.log10(bin_centers[mask])
log_p = np.log10(prob[mask])
slope, intercept, r_value, _, _ = linregress(log_s, log_p)

# plt.figure(figsize=(8,5))
# plt.loglog(bin_centers, prob, 'o', label='Дані')
# plt.loglog(bin_centers, 10**(intercept + slope*np.log10(bin_centers)), 'r--',
#            label=f'Аппроксимація: τ ≈ {-slope:.2f}')
# plt.xlabel("Розмір лавини s")
# plt.ylabel("Ймовірність P(s)")
# plt.title("Степенева залежність")
# plt.legend()
# plt.grid(True, which="both", ls="--", alpha=0.5)
# plt.show()

print(f"\nПоказник степеня τ ≈ {-slope:.2f} (R²={r_value**2:.3f})")
