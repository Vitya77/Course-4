import numpy as np


def f12(x1, x2):
    return -20 * x1**2 + 7 * x1 - 3 * x1 * x2**2 + 46 * x2 + 12


def f21(x1, x2):
    return 18 * x2**2 - 8 * x2 + 15 * x2 * x1**2 - 32 * x1 + 123


def tabulate_functions(x1_range, x2_range, step):
    x1_min, x1_max = x1_range
    x2_min, x2_max = x2_range

    x1_vals = np.arange(x1_min, x1_max + step / 2, step)
    x2_vals = np.arange(x2_min, x2_max + step / 2, step)

    col_w = [10, 10, 14, 14]
    sep = "+" + "+".join("-" * w for w in col_w) + "+"
    header = (
        f"|{'x1':^{col_w[0]}}|{'x2':^{col_w[1]}}"
        f"|{'f12':^{col_w[2]}}|{'f21':^{col_w[3]}}|"
    )

    row_count = 0
    with open("TheoryOfDecisionMaking/lab2/table.txt", "w") as f:
        f.write(sep + "\n")
        f.write(header + "\n")
        f.write(sep + "\n")
        for x1 in x1_vals:
            for x2 in x2_vals:
                v12 = f12(x1, x2)
                v21 = f21(x1, x2)
                row = (
                    f"|{x1:^{col_w[0]}.2f}|{x2:^{col_w[1]}.2f}"
                    f"|{v12:^{col_w[2]}.4f}|{v21:^{col_w[3]}.4f}|"
                )
                f.write(row + "\n")
                row_count += 1
            f.write(sep + "\n")

    print(f"Table saved to TheoryOfDecisionMaking/lab2/table.txt ({row_count} rows)")


def find_minimax(x1_range, x2_range, step, out_file):
    x1_min, x1_max = x1_range
    x2_min, x2_max = x2_range

    x1_vals = np.arange(x1_min, x1_max + step / 2, step)
    x2_vals = np.arange(x2_min, x2_max + step / 2, step)

    # Build value grid from same data as table
    grid_f12 = np.array([[f12(x1, x2) for x2 in x2_vals] for x1 in x1_vals])
    grid_f21 = np.array([[f21(x1, x2) for x2 in x2_vals] for x1 in x1_vals])

    # min by x1 of max by x2 for f12
    max_by_x2 = grid_f12.min(axis=1)  # max over x2 for each x1
    i1 = max_by_x2.argmax()
    j1 = grid_f12[i1].argmin()
    x1_star1, x2_star1 = x1_vals[i1], x2_vals[j1]

    # min by x2 of max by x1 for f21
    max_by_x1 = grid_f21.min(axis=0)  # max over x1 for each x2
    j2 = max_by_x1.argmax()
    i2 = grid_f21[:, j2].argmin()
    x1_star2, x2_star2 = x1_vals[i2], x2_vals[j2]

    with open(out_file, "a") as f:
        f.write("\nf12: min by x1 of max by x2\n")
        f.write(f"  x1* = {x1_star1:.2f}, x2* = {x2_star1:.2f}\n")
        f.write(f"  f12 = {grid_f12[i1, j1]:.4f}, f21 = {grid_f21[i1, j1]:.4f}\n")

        f.write("\nf21: min by x2 of max by x1\n")
        f.write(f"  x1* = {x1_star2:.2f}, x2* = {x2_star2:.2f}\n")
        f.write(f"  f12 = {grid_f12[i2, j2]:.4f}, f21 = {grid_f21[i2, j2]:.4f}\n")


def find_pareto_compromise(x1_range, x2_range, step, out_file):
    x1_min, x1_max = x1_range
    x2_min, x2_max = x2_range

    x1_vals = np.arange(x1_min, x1_max + step / 2, step)
    x2_vals = np.arange(x2_min, x2_max + step / 2, step)

    X1, X2 = np.meshgrid(x1_vals, x2_vals, indexing="ij")
    grid_f12 = f12(X1, X2)
    grid_f21 = f21(X1, X2)

    # f1* = max_{x1} min_{x2} f12  (player 1's maximin / guaranteed payoff)
    f1_star = grid_f12.min(axis=1).max()

    # f2* = max_{x2} min_{x1} f21  (player 2's maximin / guaranteed payoff)
    f2_star = grid_f21.min(axis=0).max()

    # Δ_i = |f_i(x1, x2) - f_i*|
    delta1 = np.abs(grid_f12 - f1_star)
    delta2 = np.abs(grid_f21 - f2_star)

    # Δ = min_{x1,x2} max_i Δ_i
    max_delta = np.maximum(delta1, delta2)
    flat_idx = max_delta.argmin()
    i_opt, j_opt = np.unravel_index(flat_idx, max_delta.shape)

    x1_opt = x1_vals[i_opt]
    x2_opt = x2_vals[j_opt]
    delta_opt = max_delta[i_opt, j_opt]

    with open(out_file, "a") as f:
        f.write("\n--- Pareto compromise (minimax deviation) ---\n")
        f.write(f"  f1* = {f1_star:.4f}  (maximin for player 1)\n")
        f.write(f"  f2* = {f2_star:.4f}  (maximin for player 2)\n")
        f.write(f"  x1* = {x1_opt:.4f}, x2* = {x2_opt:.4f}\n")
        f.write(f"  f12(x1*,x2*) = {grid_f12[i_opt, j_opt]:.4f}\n")
        f.write(f"  f21(x1*,x2*) = {grid_f21[i_opt, j_opt]:.4f}\n")
        f.write(f"  Δ1 = {delta1[i_opt, j_opt]:.4f}\n")
        f.write(f"  Δ2 = {delta2[i_opt, j_opt]:.4f}\n")
        f.write(f"  Δ  = max(Δ1, Δ2) = {delta_opt:.4f}\n")

    print(f"Pareto compromise: x1*={x1_opt:.4f}, x2*={x2_opt:.4f}, Δ={delta_opt:.4f}")


if __name__ == "__main__":
    out = "TheoryOfDecisionMaking/lab2/table.txt"
    tabulate_functions(x1_range=(0, 3), x2_range=(0, 2), step=0.01)
    find_minimax(x1_range=(0, 3), x2_range=(0, 2), step=0.01, out_file=out)
    find_pareto_compromise(x1_range=(0, 3), x2_range=(0, 2), step=0.000125, out_file=out)
    print("Done. Results appended to", out)
