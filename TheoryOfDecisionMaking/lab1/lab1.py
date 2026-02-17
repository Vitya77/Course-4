import numpy as np
import math


def f1(x):
    return 0.8 * (math.e ** (-2 * ((x - 3) ** 2)))


def f2(x):
    return 2 * (x ** -1) + 0.25


F1_STAR = 0.4
F2_STAR = 0.5

X_MIN = 1
X_MAX = 5
STEP = 0.1


def build_table(x_min, x_max, step):
    xs = np.arange(x_min, x_max + step / 2, step)
    xs = np.round(xs, 10)

    rows = []
    for x in xs:
        r1 = f1(x) / F1_STAR
        r2 = f2(x) / F2_STAR
        row_max = max(r1, r2)
        row_min = min(r1, r2)
        rows.append((x, r1, r2, row_max, row_min))

    # Find minimax (min of max column) and maximin (max of min column)
    min_max_val = min(r[3] for r in rows)
    max_min_val = max(r[4] for r in rows)

    # Print table
    header = f"{'x':>5} | {'f1/f1*':>8} | {'f2/f2*':>8} | {'max':>8} | {'min max':>8} | {'min':>8} | {'max min':>8}"
    print(header)
    print("-" * len(header))

    for x, r1, r2, row_max, row_min in rows:
        mm = f"{min_max_val:.4f}" if abs(row_max - min_max_val) < 1e-9 else "-"
        xm = f"{max_min_val:.4f}" if abs(row_min - max_min_val) < 1e-9 else "-"
        print(f"{x:5.1f} | {r1:8.4f} | {r2:8.4f} | {row_max:8.4f} | {mm:>8} | {row_min:8.4f} | {xm:>8}")

    # Print summary
    min_max_x = [r[0] for r in rows if abs(r[3] - min_max_val) < 1e-9][0]
    max_min_x = [r[0] for r in rows if abs(r[4] - max_min_val) < 1e-9][0]
    print(f"\nMinimax: x = {min_max_x:.1f}, value = {min_max_val:.4f}")
    print(f"Maximin: x = {max_min_x:.1f}, value = {max_min_val:.4f}")


if __name__ == "__main__":
    build_table(X_MIN, X_MAX, STEP)
