# forest_fire_soc.py
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
from collections import deque
import pandas as pd
import time

# Cell states
EMPTY = 0
TREE = 1
BURNING = 2

def step(grid, p, f):
    """Один крок еволюції:
    - порожні клітини виростають з ймовірністю p
    - дерево загоряється, якщо будь-який сусід горить (4- або 8-напрямів — я використав 8)
    - або дерево може загоріти від блискавки з ймовірністю f
    """
    n, m = grid.shape
    new = grid.copy()

    # Зростання дерев у порожніх
    grow = (grid == EMPTY) & (np.random.random(grid.shape) < p)
    new[grow] = TREE

    # Визначаємо, які дерева будуть зіпсовані через сусідів
    # Для швидкості використаємо згортку з сумуванням сусідів (8-напрямів)
    # але без зовнішніх бібліотек — просто вручну перевіримо сусідів
    neighbors_on_fire = np.zeros_like(grid, dtype=bool)
    for di in (-1, 0, 1):
        for dj in (-1, 0, 1):
            if di == 0 and dj == 0:
                continue
            shifted = np.zeros_like(grid, dtype=bool)
            si_start = max(0, -di)
            si_end = n - max(0, di)
            sj_start = max(0, -dj)
            sj_end = m - max(0, dj)
            shifted[si_start:si_end, sj_start:sj_end] = (grid[si_start + di:si_end + di, sj_start + dj:sj_end + dj] == BURNING)
            neighbors_on_fire |= shifted

    # Дерева, що будуть загорятись через сусідів
    will_burn = (grid == TREE) & neighbors_on_fire

    # Дерева, що загоряться від блискавки
    lightning = (grid == TREE) & (np.random.random(grid.shape) < f)

    new[will_burn | lightning] = BURNING

    # Горячі клітини стають порожніми (вигоряють)
    new[grid == BURNING] = EMPTY

    return new

def cluster_sizes(grid):
    """Повертає список розмірів кластерів дерев (не включає порожні/горячі)"""
    n, m = grid.shape
    visited = np.zeros_like(grid, dtype=bool)
    sizes = []
    for i in range(n):
        for j in range(m):
            if grid[i, j] == TREE and not visited[i, j]:
                # BFS для пошуку кластера (8-напрямів)
                q = deque()
                q.append((i, j))
                visited[i, j] = True
                cnt = 0
                while q:
                    x, y = q.popleft()
                    cnt += 1
                    for dx in (-1, 0, 1):
                        for dy in (-1, 0, 1):
                            if dx == 0 and dy == 0:
                                continue
                            nx, ny = x + dx, y + dy
                            if 0 <= nx < n and 0 <= ny < m:
                                if grid[nx, ny] == TREE and not visited[nx, ny]:
                                    visited[nx, ny] = True
                                    q.append((nx, ny))
                sizes.append(cnt)
    return sizes

def run_simulation(L=100, p=0.01, f=1e-4, steps=1000, animate=True, save_stats=False, stats_filename="forest_stats.csv"):
    grid = np.zeros((L, L), dtype=int)

    # Ініціалізація: трохи дерев, або пусто — обирай
    init_fill = 0.01
    grid[np.random.random(grid.shape) < init_fill] = TREE

    stats = {"step": [], "num_trees": [], "num_burning": [], "num_empty": [], "mean_cluster_size": [], "num_clusters": []}

    if animate:
        fig, ax = plt.subplots(figsize=(6,6))
        cmap = plt.get_cmap("viridis", 3)  # три стани
        im = ax.imshow(grid, vmin=0, vmax=2, cmap=cmap)
        ax.set_title("Forest-fire SOC model")
        ax.axis('off')

        def update(frame):
            nonlocal grid
            grid = step(grid, p, f)
            im.set_data(grid)
            # статистика
            sizes = cluster_sizes(grid)
            stats["step"].append(frame)
            stats["num_trees"].append(int((grid == TREE).sum()))
            stats["num_burning"].append(int((grid == BURNING).sum()))
            stats["num_empty"].append(int((grid == EMPTY).sum()))
            stats["mean_cluster_size"].append(float(np.mean(sizes) if sizes else 0.0))
            stats["num_clusters"].append(len(sizes))
            ax.set_title(f"Step {frame}  trees={stats['num_trees'][-1]}  clusters={stats['num_clusters'][-1]}")
            return (im,)

        anim = FuncAnimation(fig, update, frames=steps, interval=50, blit=True)
        plt.show()

        if save_stats:
            df = pd.DataFrame(stats)
            df.to_csv(stats_filename, index=False)
            print(f"Saved stats to {stats_filename}")

    else:
        # Білий цикл без анімації — тільки статистика
        for t in range(steps):
            grid = step(grid, p, f)
            sizes = cluster_sizes(grid)
            stats["step"].append(t)
            stats["num_trees"].append(int((grid == TREE).sum()))
            stats["num_burning"].append(int((grid == BURNING).sum()))
            stats["num_empty"].append(int((grid == EMPTY).sum()))
            stats["mean_cluster_size"].append(float(np.mean(sizes) if sizes else 0.0))
            stats["num_clusters"].append(len(sizes))
        if save_stats:
            df = pd.DataFrame(stats)
            df.to_csv(stats_filename, index=False)
            print(f"Saved stats to {stats_filename}")

    return stats

if __name__ == "__main__":
    # Параметри — спробуй міняти p і f:
    L = 120          # розмір решітки LxL
    p = 0.01         # ймовірність росту дерева
    f = 1e-5         # ймовірність блискавки
    steps = 1000     # кроків симуляції
    animate = True
    save_stats = False

    start = time.time()
    stats = run_simulation(L=L, p=p, f=f, steps=steps, animate=animate, save_stats=save_stats)
    print("Done in %.2f s" % (time.time() - start))
