import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

# --- параметри ---
N = 100            # кількість видів
steps = 100000       # кількість ітерацій
neighbors = 1     # по скільки сусідів оновлювати зліва/справа

# --- ініціалізація ---
fitness = np.random.rand(N)
history = [fitness.copy()]

# --- модель Бака-Снеппена ---
def step(fitness):
    i_min = np.argmin(fitness)
    indices = [(i_min + j) % N for j in range(-neighbors, neighbors + 1)]
    fitness[indices] = np.random.rand(len(indices))
    return fitness

# --- підготовка до анімації ---
fig, ax = plt.subplots(figsize=(8, 4))
bars = ax.bar(range(N), fitness, color='limegreen')
ax.set_ylim(0, 1)
ax.set_title("Модель Бака–Снеппена: Самоорганізована критичність")
ax.set_xlabel("Індекс виду")
ax.set_ylabel("Пристосованість")

def update(frame):
    global fitness
    fitness = step(fitness)
    for rect, h in zip(bars, fitness):
        rect.set_height(h)
    ax.set_title(f"Крок {frame + 1}")
    return bars

ani = FuncAnimation(fig, update, frames=steps, blit=False, interval=100, repeat=False)
plt.tight_layout()
plt.show()

# for _ in range(steps):
#     i_min = np.argmin(fitness)
#     indices = [(i_min + j) % N for j in range(-neighbors, neighbors + 1)]
#     fitness[indices] = np.random.rand(len(indices))

# # --- побудова розподілу ---
# plt.figure(figsize=(8, 5))
# plt.hist(fitness, bins=50, color='limegreen', edgecolor='black', alpha=0.8)
# plt.title(f"Розподіл пристосованостей після {steps} кроків (модель Бака–Снеппена)")
# plt.xlabel("Пристосованість (fitness)")
# plt.ylabel("Кількість видів")
# plt.axvline(np.mean(fitness), color='red', linestyle='--', label=f"Середнє = {np.mean(fitness):.3f}")
# plt.legend()
# plt.tight_layout()
# plt.show()