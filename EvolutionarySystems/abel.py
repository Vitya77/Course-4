import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import random
from mpl_toolkits.mplot3d import Axes3D  # необхідно для 3D

# Розмір сітки
N = 25

# Ініціалізація купи
sandpile = np.zeros((N, N), dtype=int)
center = (N // 2, N // 2)

# Функція "обвалу"
def topple(grid):
    unstable = True
    while unstable:
        unstable = False
        to_topple = grid >= 6
        if np.any(to_topple):
            unstable = True
            grid[to_topple] -= 6
            for i, j in zip(*np.where(to_topple)):
                if i > 0: grid[i - 1, j] += 1
                if i < N - 1: grid[i + 1, j] += 1
                if j > 0: grid[i, j - 1] += 1
                if j < N - 1: grid[i, j + 1] += 1

# Візуалізація в 3D
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

x, y = np.meshgrid(np.arange(N), np.arange(N))
surf = [ax.bar3d(x.ravel(), y.ravel(), np.zeros_like(x).ravel(),
                 dx=0.9, dy=0.9, dz=sandpile.ravel(),
                 color=plt.cm.inferno(sandpile.ravel()/4))]

ax.set_zlim(0, 6)
ax.set_title("3D Авелевa купа піску (самоорганізована критичність)")
ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel('Кількість піщинок')

def update(frame):
    randVal1 = random.randint(-1, 1)
    randVal2 = random.randint(-1, 1)
    sandpile[(center[0] + randVal1, center[1] + randVal2)] += 1
    topple(sandpile)

    # Очищаємо попередню поверхню
    ax.cla()
    ax.bar3d(x.ravel(), y.ravel(), np.zeros_like(x).ravel(),
             dx=0.9, dy=0.9, dz=sandpile.ravel(),
             color=plt.cm.inferno(sandpile.ravel()/4))
    ax.set_zlim(0, 6)
    ax.set_title("3D Авелевa купа піску (самоорганізована критичність)")
    ax.set_xlabel('X')
    ax.set_ylabel('Y')
    ax.set_zlabel('Кількість піщинок')
    return []

ani = FuncAnimation(fig, update, frames=200, interval=100, blit=False)
plt.show()
