from __future__ import annotations

import argparse
import csv
import os
import tempfile
from dataclasses import dataclass
from pathlib import Path

import numpy as np


@dataclass
class FCMResult:
    centers: np.ndarray
    membership: np.ndarray
    iterations: int
    objective_history: list[float]
    centers_history: list[np.ndarray]
    membership_history: list[np.ndarray]


def generate_demo_data(seed: int = 42) -> np.ndarray:
    rng = np.random.default_rng(seed)
    cluster_a = rng.normal(loc=(-2.0, 1.5), scale=(0.9, 0.7), size=(60, 2))
    cluster_b = rng.normal(loc=(1.2, -1.0), scale=(0.8, 1.1), size=(60, 2))
    cluster_c = rng.normal(loc=(3.5, 2.5), scale=(1.0, 0.8), size=(60, 2))
    return np.vstack((cluster_a, cluster_b, cluster_c))


def initialize_membership(
    n_samples: int, n_clusters: int, rng: np.random.Generator
) -> np.ndarray:
    membership = rng.random((n_samples, n_clusters))
    membership /= membership.sum(axis=1, keepdims=True)
    return membership


def compute_centers(data: np.ndarray, membership: np.ndarray, m: float) -> np.ndarray:
    weights = membership**m
    numerator = weights.T @ data
    denominator = weights.sum(axis=0)[:, np.newaxis]
    return numerator / denominator


def compute_distances(data: np.ndarray, centers: np.ndarray) -> np.ndarray:
    return np.linalg.norm(data[:, np.newaxis, :] - centers[np.newaxis, :, :], axis=2)


def update_membership(distances: np.ndarray, m: float) -> np.ndarray:
    power = 2.0 / (m - 1.0)
    n_samples, n_clusters = distances.shape
    membership = np.zeros((n_samples, n_clusters), dtype=float)

    for sample_index in range(n_samples):
        zero_distance_clusters = np.where(distances[sample_index] == 0.0)[0]
        if zero_distance_clusters.size:
            membership[sample_index, zero_distance_clusters] = (
                1.0 / zero_distance_clusters.size
            )
            continue

        ratios = (
            distances[sample_index][:, np.newaxis]
            / distances[sample_index][np.newaxis, :]
        ) ** power
        membership[sample_index] = 1.0 / ratios.sum(axis=1)

    return membership


def objective_function(
    membership: np.ndarray, distances: np.ndarray, m: float
) -> float:
    return float(np.sum((membership**m) * (distances**2)))


def fuzzy_c_means(
    data: np.ndarray,
    n_clusters: int,
    m: float = 2.0,
    max_iter: int = 150,
    tol: float = 1e-4,
    seed: int = 42,
) -> FCMResult:
    if m <= 1.0:
        raise ValueError("The fuzziness coefficient m must be greater than 1.")

    rng = np.random.default_rng(seed)
    membership = initialize_membership(len(data), n_clusters, rng)
    objective_history: list[float] = []
    centers_history: list[np.ndarray] = []
    membership_history: list[np.ndarray] = []

    for iteration in range(1, max_iter + 1):
        centers_before = compute_centers(data, membership, m)
        distances_before = compute_distances(data, centers_before)
        new_membership = update_membership(distances_before, m)
        shift = np.max(np.abs(new_membership - membership))

        membership = new_membership
        centers_after = compute_centers(data, membership, m)
        distances_after = compute_distances(data, centers_after)
        objective_value = objective_function(membership, distances_after, m)

        membership_history.append(membership.copy())
        centers_history.append(centers_after.copy())
        objective_history.append(objective_value)

        if shift < tol:
            break

    return FCMResult(
        centers=centers_history[-1],
        membership=membership_history[-1],
        iterations=iteration,
        objective_history=objective_history,
        centers_history=centers_history,
        membership_history=membership_history,
    )


def save_membership_table(
    data: np.ndarray, membership: np.ndarray, output_path: Path
) -> None:
    predicted_cluster = membership.argmax(axis=1)
    confidence = membership.max(axis=1)
    fieldnames = ["x", "y", "predicted_cluster", "max_membership"] + [
        f"membership_cluster_{cluster_index + 1}"
        for cluster_index in range(membership.shape[1])
    ]

    with output_path.open("w", newline="", encoding="utf-8") as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()

        for point, memberships, cluster, point_confidence in zip(
            data, membership, predicted_cluster, confidence
        ):
            row = {
                "x": f"{point[0]:.6f}",
                "y": f"{point[1]:.6f}",
                "predicted_cluster": int(cluster) + 1,
                "max_membership": f"{point_confidence:.6f}",
            }
            for membership_index, membership_value in enumerate(memberships, start=1):
                row[f"membership_cluster_{membership_index}"] = (
                    f"{membership_value:.6f}"
                )
            writer.writerow(row)


def ensure_matplotlib():
    cache_dir = Path(tempfile.gettempdir()) / "course4_matplotlib_cache"
    cache_dir.mkdir(parents=True, exist_ok=True)
    os.environ.setdefault("MPLCONFIGDIR", str(cache_dir))

    try:
        import matplotlib.pyplot as plt
        from matplotlib import colors as mcolors
    except ImportError as error:
        raise SystemExit(
            "matplotlib is required. Install it with: python3 -m pip install --user matplotlib"
        ) from error

    return plt, mcolors


def membership_to_rgba(
    membership: np.ndarray, palette: list[str], mcolors
) -> np.ndarray:
    cluster_ids = membership.argmax(axis=1)
    confidence = membership.max(axis=1)
    facecolors = np.zeros((len(membership), 4), dtype=float)

    for sample_index, cluster_id in enumerate(cluster_ids):
        red, green, blue, _ = mcolors.to_rgba(palette[cluster_id % len(palette)])
        facecolors[sample_index] = (
            red,
            green,
            blue,
            0.25 + 0.75 * confidence[sample_index],
        )

    return facecolors


def configure_axes(data: np.ndarray, result: FCMResult, cluster_ax, objective_ax) -> None:
    all_centers = np.vstack(result.centers_history)
    min_x = min(float(data[:, 0].min()), float(all_centers[:, 0].min())) - 0.8
    max_x = max(float(data[:, 0].max()), float(all_centers[:, 0].max())) + 0.8
    min_y = min(float(data[:, 1].min()), float(all_centers[:, 1].min())) - 0.8
    max_y = max(float(data[:, 1].max()), float(all_centers[:, 1].max())) + 0.8

    objective_values = np.array(result.objective_history, dtype=float)
    objective_padding = max(1.0, 0.08 * float(objective_values.max() - objective_values.min()))

    cluster_ax.set_xlim(min_x, max_x)
    cluster_ax.set_ylim(min_y, max_y)
    cluster_ax.set_xlabel("Feature 1")
    cluster_ax.set_ylabel("Feature 2")
    cluster_ax.set_title("Clusters, centers and membership confidence")
    cluster_ax.grid(alpha=0.25)

    objective_ax.set_xlim(1, max(2, result.iterations))
    objective_ax.set_ylim(
        float(objective_values.min()) - objective_padding,
        float(objective_values.max()) + objective_padding,
    )
    objective_ax.set_xlabel("Iteration")
    objective_ax.set_ylabel("J(U, C)")
    objective_ax.set_title("Objective function convergence")
    objective_ax.grid(alpha=0.25)


def build_visualization(data: np.ndarray, result: FCMResult):
    plt, mcolors = ensure_matplotlib()
    palette = ["tab:blue", "tab:orange", "tab:green", "tab:red", "tab:purple"]

    plt.style.use("seaborn-v0_8-whitegrid")
    fig, (cluster_ax, objective_ax) = plt.subplots(1, 2, figsize=(13, 6))
    configure_axes(data, result, cluster_ax, objective_ax)

    initial_membership = result.membership_history[0]
    initial_centers = result.centers_history[0]

    points_scatter = cluster_ax.scatter(
        data[:, 0],
        data[:, 1],
        s=70,
        c=membership_to_rgba(initial_membership, palette, mcolors),
        edgecolors="black",
        linewidths=0.45,
    )
    centers_scatter = cluster_ax.scatter(
        initial_centers[:, 0],
        initial_centers[:, 1],
        s=260,
        c="gold",
        marker="X",
        edgecolors="black",
        linewidths=1.2,
        label="Cluster centers",
    )
    cluster_ax.legend(loc="upper right")

    objective_line, = objective_ax.plot([], [], color="tab:blue", marker="o", linewidth=2)
    objective_marker, = objective_ax.plot([], [], "o", color="crimson", markersize=8)

    status_text = fig.text(
        0.13,
        0.95,
        "",
        fontsize=12,
        fontweight="bold",
    )

    return {
        "plt": plt,
        "mcolors": mcolors,
        "palette": palette,
        "fig": fig,
        "cluster_ax": cluster_ax,
        "objective_ax": objective_ax,
        "points_scatter": points_scatter,
        "centers_scatter": centers_scatter,
        "objective_line": objective_line,
        "objective_marker": objective_marker,
        "status_text": status_text,
    }


def update_visualization(state: dict[str, object], result: FCMResult, frame_index: int) -> None:
    membership = result.membership_history[frame_index]
    centers = result.centers_history[frame_index]
    objective_values = result.objective_history[: frame_index + 1]
    iterations = np.arange(1, frame_index + 2)
    confidence = membership.max(axis=1)

    points_scatter = state["points_scatter"]
    centers_scatter = state["centers_scatter"]
    objective_line = state["objective_line"]
    objective_marker = state["objective_marker"]
    status_text = state["status_text"]

    points_scatter.set_facecolors(
        membership_to_rgba(membership, state["palette"], state["mcolors"])
    )
    centers_scatter.set_offsets(centers)
    objective_line.set_data(iterations, objective_values)
    objective_marker.set_data([iterations[-1]], [objective_values[-1]])
    status_text.set_text(
        "Iteration "
        f"{frame_index + 1}/{result.iterations} | "
        f"J(U, C) = {objective_values[-1]:.4f} | "
        f"Average max membership = {confidence.mean():.4f}"
    )


def animate_clustering(data: np.ndarray, result: FCMResult, interval: float) -> None:
    state = build_visualization(data, result)
    plt = state["plt"]

    plt.ion()
    for frame_index in range(result.iterations):
        update_visualization(state, result, frame_index)
        state["fig"].canvas.draw_idle()
        plt.pause(interval)

    plt.ioff()
    plt.show()


def save_final_plot(data: np.ndarray, result: FCMResult, output_path: Path) -> None:
    state = build_visualization(data, result)
    update_visualization(state, result, result.iterations - 1)
    state["fig"].tight_layout(rect=(0, 0, 1, 0.93))
    state["fig"].savefig(output_path, dpi=180, bbox_inches="tight")
    state["plt"].close(state["fig"])


def print_summary(data: np.ndarray, result: FCMResult) -> None:
    np.set_printoptions(precision=4, suppress=True)
    predicted_cluster = result.membership.argmax(axis=1) + 1
    confidence = result.membership.max(axis=1)

    print("Fuzzy C-Means demonstration")
    print(f"Objects: {len(data)}")
    print(f"Clusters: {result.centers.shape[0]}")
    print(f"Iterations: {result.iterations}")
    print(f"Final objective value: {result.objective_history[-1]:.6f}")
    print("\nCluster centers:")
    print(result.centers)

    preview_count = 10
    print(f"\nFirst {preview_count} objects and their membership values:")
    for index in range(preview_count):
        memberships = ", ".join(
            f"u{cluster_index + 1}={value:.4f}"
            for cluster_index, value in enumerate(result.membership[index])
        )
        print(
            f"Object {index + 1:>2}: point={data[index]}, "
            f"cluster={predicted_cluster[index]}, confidence={confidence[index]:.4f}, "
            f"{memberships}"
        )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Demonstration of the Fuzzy C-Means clustering algorithm."
    )
    parser.add_argument("--clusters", type=int, default=3, help="Number of clusters.")
    parser.add_argument("--m", type=float, default=2.0, help="Fuzziness coefficient.")
    parser.add_argument(
        "--max-iter", type=int, default=150, help="Maximum number of iterations."
    )
    parser.add_argument(
        "--tol",
        type=float,
        default=1e-4,
        help="Convergence tolerance for the membership matrix.",
    )
    parser.add_argument(
        "--seed",
        type=int,
        default=42,
        help="Random seed for data generation and initialization.",
    )
    parser.add_argument(
        "--interval",
        type=float,
        default=0.55,
        help="Pause between animation frames in seconds.",
    )
    parser.add_argument(
        "--no-animate",
        action="store_true",
        help="Skip the live matplotlib animation and only save the final plot.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    data = generate_demo_data(seed=args.seed)
    result = fuzzy_c_means(
        data=data,
        n_clusters=args.clusters,
        m=args.m,
        max_iter=args.max_iter,
        tol=args.tol,
        seed=args.seed,
    )

    base_dir = Path(__file__).resolve().parent
    csv_path = base_dir / "fuzzy_c_means_membership.csv"
    png_plot_path = base_dir / "fuzzy_c_means_result.png"

    save_membership_table(data, result.membership, csv_path)
    save_final_plot(data, result, png_plot_path)
    print_summary(data, result)

    print(f"\nMembership table saved to: {csv_path}")
    print(f"Matplotlib plot saved to: {png_plot_path}")
    if args.no_animate:
        print("Live animation skipped because --no-animate was provided.")
    else:
        print("Opening live matplotlib animation window...")
        animate_clustering(data, result, interval=args.interval)


if __name__ == "__main__":
    main()
