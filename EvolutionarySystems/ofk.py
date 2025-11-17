import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

# Параметри моделі
N = 25          # розмір решітки
alpha = 0.22    # коефіцієнт розподілу
F_th = 1.0      # поріг
drive_rate = 0.002  # швидкість підкачки

# Ініціалізація
stress = np.random.rand(N, N) * F_th
toppled = np.zeros_like(stress, dtype=bool)
unstable = np.zeros_like(stress, dtype=bool)
driving = True  # стан: підкачка чи лавина

# --- Візуалізація ---
fig, ax = plt.subplots()
im = ax.imshow(stress, cmap='inferno', vmin=0, vmax=F_th)
ax.set_title("Модель Оламі–Федера–Крістенсена (OFC)")
ax.axis('off')

def update(frame):
    global stress, unstable, toppled, driving

    if driving:
        # рівномірно підкачуємо
        stress += drive_rate
        if np.max(stress) >= F_th:
            unstable = stress >= F_th
            toppled[:] = False
            driving = False  # переходимо до лавини
    else:
        # один “шар” лавини
        new_toppled = np.argwhere(unstable)
        if len(new_toppled) == 0:
            driving = True  # лавина закінчилась
        else:
            # масив дельт
            d = np.zeros_like(stress)
            for (i, j) in new_toppled:
                redistributed = alpha * stress[i, j]
                stress[i, j] = 0.0
                toppled[i, j] = True
                if i > 0: d[i - 1, j] += redistributed
                if i < N - 1: d[i + 1, j] += redistributed
                if j > 0: d[i, j - 1] += redistributed
                if j < N - 1: d[i, j + 1] += redistributed
            stress += d
            unstable = stress >= F_th

    # малюємо: активні клітинки підсвічуємо
    img = stress.copy()
    img[toppled] = F_th * 1.2  # активні – яскраві
    im.set_data(img)
    return [im]

ani = FuncAnimation(fig, update, interval=30, blit=True)
plt.show()
