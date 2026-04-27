from __future__ import annotations

import argparse
import random
import urllib.request
import zipfile
from dataclasses import dataclass
from pathlib import Path

import cv2
import numpy as np
import torch
from torch import nn
from torch.utils.data import DataLoader, TensorDataset


SUPPORTED_EXTENSIONS = {".png", ".jpg", ".jpeg", ".bmp", ".pgm"}
ATT_FACES_URL = "https://www.cl.cam.ac.uk/Research/DTG/attarchive/pub/data/att_faces.zip"


@dataclass
class DatasetBundle:
    images: np.ndarray
    labels: np.ndarray
    class_names: list[str]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Лабораторна №5: класифікація облич за допомогою SVM, Random Forest, KNN та CNN.",
    )
    parser.add_argument(
        "--dataset",
        type=Path,
        default=Path("att_faces"),
        help="Папка з підпапками класів (наприклад: dataset/person_1/*.jpg).",
    )
    parser.add_argument(
        "--download-att-faces",
        action="store_true",
        help="Автоматично завантажити датасет AT&T Faces, якщо папка відсутня.",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path("lab5_output"),
        help="Папка для збереження звітів і візуалізацій.",
    )
    parser.add_argument("--image-size", type=int, default=32, help="Розмір квадратного зображення після resize.")
    parser.add_argument("--test-size", type=float, default=0.2, help="Частка тестової вибірки.")
    parser.add_argument("--seed", type=int, default=42, help="Зерно випадковості.")
    parser.add_argument("--knn-k", type=int, default=3, help="Кількість сусідів для KNN.")
    parser.add_argument("--rf-trees", type=int, default=120, help="Кількість дерев у Random Forest.")
    parser.add_argument("--rf-depth", type=int, default=14, help="Максимальна глибина дерева Random Forest.")
    parser.add_argument("--cnn-epochs", type=int, default=30, help="Кількість епох навчання CNN.")
    parser.add_argument("--cnn-lr", type=float, default=0.001, help="Крок навчання CNN.")
    parser.add_argument("--cnn-batch-size", type=int, default=16, help="Розмір batch для CNN.")
    parser.add_argument(
        "--max-classes",
        type=int,
        default=None,
        help="За потреби обмежити кількість класів для швидших експериментів.",
    )
    parser.add_argument(
        "--images-per-class",
        type=int,
        default=None,
        help="За потреби обмежити кількість зображень на клас.",
    )
    return parser.parse_args()


def seed_everything(seed: int) -> None:
    random.seed(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)


def maybe_download_att_faces(dataset_dir: Path) -> None:
    if dataset_dir.exists():
        return

    dataset_dir.parent.mkdir(parents=True, exist_ok=True)
    archive_path = dataset_dir.parent / "att_faces.zip"
    print(f"\nЗавантаження датасету AT&T Faces з {ATT_FACES_URL}")
    urllib.request.urlretrieve(ATT_FACES_URL, archive_path)

    with zipfile.ZipFile(archive_path, "r") as archive:
        archive.extractall(dataset_dir.parent)

    dataset_dir.mkdir(parents=True, exist_ok=True)
    for path in sorted(dataset_dir.parent.iterdir()):
        if not path.is_dir():
            continue
        if not path.name.startswith("s"):
            continue
        if not path.name[1:].isdigit():
            continue
        target = dataset_dir / path.name
        if path != target and not target.exists():
            path.rename(target)

    readme_path = dataset_dir.parent / "README"
    if readme_path.exists():
        target_readme = dataset_dir / "README.txt"
        if not target_readme.exists():
            readme_path.rename(target_readme)

    if archive_path.exists():
        archive_path.unlink()


def collect_class_dirs(dataset_dir: Path, max_classes: int | None) -> list[Path]:
    class_dirs: list[Path] = []
    for path in sorted(dataset_dir.iterdir()):
        if not path.is_dir():
            continue
        if any(file_path.suffix.lower() in SUPPORTED_EXTENSIONS for file_path in path.iterdir() if file_path.is_file()):
            class_dirs.append(path)

    if max_classes is not None:
        class_dirs = class_dirs[: max(max_classes, 0)]
    return class_dirs


def load_face_image(image_path: Path, image_size: int) -> np.ndarray | None:
    image = cv2.imread(str(image_path), cv2.IMREAD_GRAYSCALE)
    if image is None:
        return None
    image = cv2.resize(image, (image_size, image_size), interpolation=cv2.INTER_AREA)
    return image.astype(np.float32) / 255.0


def load_dataset(dataset_dir: Path, image_size: int, max_classes: int | None, images_per_class: int | None) -> DatasetBundle:
    if not dataset_dir.exists():
        raise FileNotFoundError(f"Папка датасету не знайдена: {dataset_dir}")

    class_dirs = collect_class_dirs(dataset_dir, max_classes)
    if len(class_dirs) < 2:
        raise ValueError("Потрібно щонайменше 2 класи з фотографіями облич.")

    images: list[np.ndarray] = []
    labels: list[int] = []
    class_names: list[str] = []

    for label_index, class_dir in enumerate(class_dirs):
        image_paths = [
            path
            for path in sorted(class_dir.iterdir())
            if path.is_file() and path.suffix.lower() in SUPPORTED_EXTENSIONS
        ]
        if images_per_class is not None:
            image_paths = image_paths[: max(images_per_class, 0)]
        if len(image_paths) < 2:
            continue

        class_names.append(class_dir.name)
        current_label = len(class_names) - 1
        for image_path in image_paths:
            image = load_face_image(image_path, image_size)
            if image is None:
                continue
            images.append(image)
            labels.append(current_label)

    if len(class_names) < 2:
        raise ValueError("Після фільтрації залишилось менше 2 валідних класів.")

    return DatasetBundle(
        images=np.stack(images).astype(np.float32),
        labels=np.asarray(labels, dtype=np.int32),
        class_names=class_names,
    )


def stratified_split(labels: np.ndarray, test_size: float, seed: int) -> tuple[np.ndarray, np.ndarray]:
    rng = np.random.default_rng(seed)
    train_indices: list[int] = []
    test_indices: list[int] = []

    for class_id in np.unique(labels):
        class_indices = np.flatnonzero(labels == class_id)
        if len(class_indices) < 2:
            raise ValueError(f"Клас {class_id} має замало прикладів для train/test split.")

        shuffled = class_indices.copy()
        rng.shuffle(shuffled)

        test_count = int(round(len(shuffled) * test_size))
        test_count = min(max(test_count, 1), len(shuffled) - 1)
        test_indices.extend(shuffled[:test_count].tolist())
        train_indices.extend(shuffled[test_count:].tolist())

    rng.shuffle(train_indices)
    rng.shuffle(test_indices)
    return np.asarray(train_indices, dtype=np.int32), np.asarray(test_indices, dtype=np.int32)


def accuracy_score(y_true: np.ndarray, y_pred: np.ndarray) -> float:
    return float(np.mean(y_true == y_pred))


def confusion_matrix(y_true: np.ndarray, y_pred: np.ndarray, num_classes: int) -> np.ndarray:
    matrix = np.zeros((num_classes, num_classes), dtype=np.int32)
    for truth, pred in zip(y_true, y_pred, strict=True):
        matrix[int(truth), int(pred)] += 1
    return matrix


def render_prediction_montage(
    images: np.ndarray,
    y_true: np.ndarray,
    y_pred: np.ndarray,
    class_names: list[str],
    title: str,
) -> np.ndarray:
    count = min(len(images), 12)
    tile = 96
    header_h = 54
    rows = 3
    cols = 4
    canvas = np.full((header_h + rows * tile, cols * tile, 3), 245, dtype=np.uint8)

    cv2.rectangle(canvas, (0, 0), (canvas.shape[1], header_h), (255, 255, 255), -1)
    cv2.putText(canvas, title, (12, 24), cv2.FONT_HERSHEY_SIMPLEX, 0.65, (20, 20, 20), 2, cv2.LINE_AA)
    cv2.putText(
        canvas,
        "green = correct, red = mistake",
        (12, 45),
        cv2.FONT_HERSHEY_SIMPLEX,
        0.45,
        (60, 60, 60),
        1,
        cv2.LINE_AA,
    )

    for index in range(count):
        row = index // cols
        col = index % cols
        x0 = col * tile
        y0 = header_h + row * tile

        image = (images[index] * 255.0).clip(0, 255).astype(np.uint8)
        image = cv2.resize(image, (tile, tile), interpolation=cv2.INTER_NEAREST)
        image_bgr = cv2.cvtColor(image, cv2.COLOR_GRAY2BGR)
        canvas[y0 : y0 + tile, x0 : x0 + tile] = image_bgr

        truth = class_names[int(y_true[index])]
        pred = class_names[int(y_pred[index])]
        color = (30, 170, 30) if truth == pred else (20, 20, 220)
        cv2.rectangle(canvas, (x0 + 1, y0 + 1), (x0 + tile - 2, y0 + tile - 2), color, 2)
        cv2.putText(
            canvas,
            f"T:{truth}",
            (x0 + 4, y0 + 16),
            cv2.FONT_HERSHEY_SIMPLEX,
            0.38,
            (255, 255, 255),
            1,
            cv2.LINE_AA,
        )
        cv2.putText(
            canvas,
            f"P:{pred}",
            (x0 + 4, y0 + 32),
            cv2.FONT_HERSHEY_SIMPLEX,
            0.38,
            (255, 255, 255),
            1,
            cv2.LINE_AA,
        )
    return canvas


def save_text(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


def flatten_images(images: np.ndarray) -> np.ndarray:
    return images.reshape(len(images), -1).astype(np.float32)


def train_knn(train_x: np.ndarray, train_y: np.ndarray, k: int):
    model = cv2.ml.KNearest_create()
    model.setDefaultK(max(k, 1))
    model.train(train_x, cv2.ml.ROW_SAMPLE, train_y.astype(np.float32))
    return model


def predict_knn(model, test_x: np.ndarray, k: int) -> np.ndarray:
    _, results, _, _ = model.findNearest(test_x, max(k, 1))
    return results.flatten().astype(np.int32)


def train_svm(train_x: np.ndarray, train_y: np.ndarray):
    model = cv2.ml.SVM_create()
    model.setType(cv2.ml.SVM_C_SVC)
    model.setKernel(cv2.ml.SVM_LINEAR)
    model.setC(2.5)
    model.train(train_x, cv2.ml.ROW_SAMPLE, train_y.astype(np.int32))
    return model


def predict_svm(model, test_x: np.ndarray) -> np.ndarray:
    _, results = model.predict(test_x)
    return results.flatten().astype(np.int32)


def train_random_forest(train_x: np.ndarray, train_y: np.ndarray, tree_count: int, depth: int):
    model = cv2.ml.RTrees_create()
    model.setMaxDepth(max(depth, 2))
    model.setMinSampleCount(2)
    model.setRegressionAccuracy(0.0)
    model.setUseSurrogates(False)
    model.setMaxCategories(64)
    model.setPriors(np.array([]))
    model.setCalculateVarImportance(True)
    model.setActiveVarCount(0)
    model.setTermCriteria((cv2.TERM_CRITERIA_MAX_ITER, max(tree_count, 10), 0))
    model.train(train_x, cv2.ml.ROW_SAMPLE, train_y.astype(np.int32))
    return model


def predict_random_forest(model, test_x: np.ndarray) -> np.ndarray:
    _, results = model.predict(test_x)
    return results.flatten().astype(np.int32)


class SmallFaceCNN(nn.Module):
    def __init__(self, input_size: int, num_classes: int) -> None:
        super().__init__()
        pooled_size = input_size // 4
        self.features = nn.Sequential(
            nn.Conv2d(1, 8, kernel_size=3, padding=1),
            nn.ReLU(),
            nn.MaxPool2d(kernel_size=2),
            nn.Conv2d(8, 16, kernel_size=3, padding=1),
            nn.ReLU(),
            nn.MaxPool2d(kernel_size=2),
        )
        self.classifier = nn.Sequential(
            nn.Flatten(),
            nn.Linear(16 * pooled_size * pooled_size, 64),
            nn.ReLU(),
            nn.Linear(64, num_classes),
        )

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        x = self.features(x)
        return self.classifier(x)


def evaluate_torch_model(model: nn.Module, features: torch.Tensor, labels: torch.Tensor) -> tuple[float, np.ndarray]:
    model.eval()
    with torch.no_grad():
        logits = model(features)
        predictions = torch.argmax(logits, dim=1)
    preds_np = predictions.cpu().numpy().astype(np.int32)
    labels_np = labels.cpu().numpy().astype(np.int32)
    return accuracy_score(labels_np, preds_np), preds_np


def train_torch_cnn(
    train_images: np.ndarray,
    train_labels: np.ndarray,
    test_images: np.ndarray,
    test_labels: np.ndarray,
    input_size: int,
    num_classes: int,
    epochs: int,
    lr: float,
    batch_size: int,
) -> tuple[nn.Module, list[tuple[int, float, float, float]], np.ndarray]:
    device = torch.device("cpu")
    model = SmallFaceCNN(input_size=input_size, num_classes=num_classes).to(device)
    criterion = nn.CrossEntropyLoss()
    optimizer = torch.optim.Adam(model.parameters(), lr=lr)

    train_x = torch.tensor(train_images[:, None, :, :], dtype=torch.float32, device=device)
    test_x = torch.tensor(test_images[:, None, :, :], dtype=torch.float32, device=device)
    train_y = torch.tensor(train_labels, dtype=torch.long, device=device)
    test_y = torch.tensor(test_labels, dtype=torch.long, device=device)

    generator = torch.Generator(device="cpu")
    generator.manual_seed(torch.initial_seed())
    loader = DataLoader(
        TensorDataset(train_x, train_y),
        batch_size=batch_size,
        shuffle=True,
        generator=generator,
    )

    history: list[tuple[int, float, float, float]] = []
    best_test_acc = -1.0
    best_state = {key: value.detach().cpu().clone() for key, value in model.state_dict().items()}

    for epoch in range(1, epochs + 1):
        model.train()
        batch_losses: list[float] = []

        for batch_x, batch_y in loader:
            optimizer.zero_grad()
            logits = model(batch_x)
            loss = criterion(logits, batch_y)
            loss.backward()
            optimizer.step()
            batch_losses.append(float(loss.item()))

        train_acc, _ = evaluate_torch_model(model, train_x, train_y)
        test_acc, test_pred = evaluate_torch_model(model, test_x, test_y)
        mean_loss = float(np.mean(batch_losses)) if batch_losses else 0.0
        history.append((epoch, mean_loss, train_acc, test_acc))

        if test_acc >= best_test_acc:
            best_test_acc = test_acc
            best_state = {key: value.detach().cpu().clone() for key, value in model.state_dict().items()}

        print(
            f"  CNN epoch {epoch:02d}/{epochs} | "
            f"loss={mean_loss:.4f} | train_acc={train_acc:.4f} | test_acc={test_acc:.4f}"
        )

    model.load_state_dict(best_state)
    _, best_pred = evaluate_torch_model(model, test_x, test_y)
    return model, history, best_pred


def render_history_chart(history: list[tuple[int, float, float, float]]) -> np.ndarray:
    width, height = 760, 420
    canvas = np.full((height, width, 3), 255, dtype=np.uint8)
    margin_left, margin_right, margin_top, margin_bottom = 55, 20, 30, 45
    plot_w = width - margin_left - margin_right
    plot_h = height - margin_top - margin_bottom
    x0 = margin_left
    y0 = height - margin_bottom

    cv2.line(canvas, (x0, margin_top), (x0, y0), (0, 0, 0), 1)
    cv2.line(canvas, (x0, y0), (x0 + plot_w, y0), (0, 0, 0), 1)
    cv2.putText(canvas, "CNN training history", (margin_left, 20), cv2.FONT_HERSHEY_SIMPLEX, 0.7, (0, 0, 0), 2)

    epochs = [row[0] for row in history]
    losses = [row[1] for row in history]
    train_accs = [row[2] for row in history]
    test_accs = [row[3] for row in history]
    max_epoch = max(epochs) if epochs else 1
    max_loss = max(max(losses, default=1.0), 1.0)

    def to_x(epoch: int) -> int:
        if max_epoch == 1:
            return x0 + plot_w // 2
        return int(x0 + (epoch - 1) / (max_epoch - 1) * plot_w)

    def to_y(value: float, max_value: float) -> int:
        return int(y0 - (value / max_value) * plot_h)

    for tick in range(6):
        y = int(margin_top + tick / 5 * plot_h)
        cv2.line(canvas, (x0, y), (x0 + plot_w, y), (230, 230, 230), 1)

    for epoch in epochs:
        x = to_x(epoch)
        cv2.line(canvas, (x, y0), (x, y0 + 4), (0, 0, 0), 1)
        cv2.putText(canvas, str(epoch), (x - 7, y0 + 18), cv2.FONT_HERSHEY_SIMPLEX, 0.4, (0, 0, 0), 1)

    def draw_curve(values: list[float], color: tuple[int, int, int], max_value: float) -> None:
        points = [(to_x(epoch), to_y(value, max_value)) for epoch, value in zip(epochs, values, strict=True)]
        for p1, p2 in zip(points, points[1:], strict=False):
            cv2.line(canvas, p1, p2, color, 2, cv2.LINE_AA)
        for point in points:
            cv2.circle(canvas, point, 3, color, -1)

    draw_curve(losses, (200, 70, 40), max_loss)
    draw_curve(train_accs, (40, 140, 40), 1.0)
    draw_curve(test_accs, (40, 80, 200), 1.0)

    cv2.putText(canvas, "loss", (width - 160, 44), cv2.FONT_HERSHEY_SIMPLEX, 0.5, (200, 70, 40), 2)
    cv2.putText(canvas, "train_acc", (width - 160, 66), cv2.FONT_HERSHEY_SIMPLEX, 0.5, (40, 140, 40), 2)
    cv2.putText(canvas, "test_acc", (width - 160, 88), cv2.FONT_HERSHEY_SIMPLEX, 0.5, (40, 80, 200), 2)
    return canvas


def summarize_model(
    model_name: str,
    y_true: np.ndarray,
    y_pred: np.ndarray,
    class_names: list[str],
) -> str:
    acc = accuracy_score(y_true, y_pred)
    matrix = confusion_matrix(y_true, y_pred, len(class_names))
    lines = [f"{model_name}", f"accuracy: {acc:.4f}", "confusion matrix:"]
    for row in matrix:
        lines.append(" ".join(f"{int(value):3d}" for value in row))
    return "\n".join(lines)


def main() -> None:
    args = parse_args()
    seed_everything(args.seed)

    dataset_path = args.dataset
    if args.download_att_faces and not dataset_path.exists():
        maybe_download_att_faces(dataset_path)

    dataset = load_dataset(
        dataset_path,
        image_size=args.image_size,
        max_classes=args.max_classes,
        images_per_class=args.images_per_class,
    )

    train_idx, test_idx = stratified_split(dataset.labels, args.test_size, args.seed)
    train_images = dataset.images[train_idx]
    test_images = dataset.images[test_idx]
    train_labels = dataset.labels[train_idx]
    test_labels = dataset.labels[test_idx]

    output_dir = args.output_dir
    output_dir.mkdir(parents=True, exist_ok=True)

    print(f"\nДатасет: {dataset_path.resolve()}")
    print(f"Класів: {len(dataset.class_names)}")
    print(f"Зображень: {len(dataset.images)}")
    print(f"Train/Test: {len(train_images)} / {len(test_images)}")
    print(f"Результати будуть збережені в: {output_dir.resolve()}")

    train_flat = flatten_images(train_images)
    test_flat = flatten_images(test_images)

    print("\nНавчання класичних моделей...")
    knn_model = train_knn(train_flat, train_labels, args.knn_k)
    svm_model = train_svm(train_flat, train_labels)
    rf_model = train_random_forest(train_flat, train_labels, args.rf_trees, args.rf_depth)

    knn_pred = predict_knn(knn_model, test_flat, args.knn_k)
    svm_pred = predict_svm(svm_model, test_flat)
    rf_pred = predict_random_forest(rf_model, test_flat)

    save_text(output_dir / "knn_metrics.txt", summarize_model("KNN", test_labels, knn_pred, dataset.class_names))
    save_text(output_dir / "svm_metrics.txt", summarize_model("SVM", test_labels, svm_pred, dataset.class_names))
    save_text(
        output_dir / "random_forest_metrics.txt",
        summarize_model("Random Forest", test_labels, rf_pred, dataset.class_names),
    )

    cv2.imwrite(
        str(output_dir / "knn_predictions.png"),
        render_prediction_montage(test_images, test_labels, knn_pred, dataset.class_names, "KNN predictions"),
    )
    cv2.imwrite(
        str(output_dir / "svm_predictions.png"),
        render_prediction_montage(test_images, test_labels, svm_pred, dataset.class_names, "SVM predictions"),
    )
    cv2.imwrite(
        str(output_dir / "random_forest_predictions.png"),
        render_prediction_montage(
            test_images,
            test_labels,
            rf_pred,
            dataset.class_names,
            "Random Forest predictions",
        ),
    )

    print(f"  KNN accuracy: {accuracy_score(test_labels, knn_pred):.4f}")
    print(f"  SVM accuracy: {accuracy_score(test_labels, svm_pred):.4f}")
    print(f"  RF  accuracy: {accuracy_score(test_labels, rf_pred):.4f}")

    print("\nНавчання CNN...")
    _, history, cnn_pred = train_torch_cnn(
        train_images=train_images,
        train_labels=train_labels,
        test_images=test_images,
        test_labels=test_labels,
        input_size=args.image_size,
        num_classes=len(dataset.class_names),
        epochs=args.cnn_epochs,
        lr=args.cnn_lr,
        batch_size=args.cnn_batch_size,
    )

    history_lines = ["epoch,loss,train_acc,test_acc"]
    for epoch, loss, train_acc, test_acc in history:
        history_lines.append(f"{epoch},{loss:.6f},{train_acc:.6f},{test_acc:.6f}")
    save_text(output_dir / "cnn_history.csv", "\n".join(history_lines))
    save_text(output_dir / "cnn_metrics.txt", summarize_model("CNN", test_labels, cnn_pred, dataset.class_names))

    cv2.imwrite(str(output_dir / "cnn_history.png"), render_history_chart(history))
    cv2.imwrite(
        str(output_dir / "cnn_predictions.png"),
        render_prediction_montage(test_images, test_labels, cnn_pred, dataset.class_names, "CNN predictions"),
    )

    print(f"  CNN accuracy: {accuracy_score(test_labels, cnn_pred):.4f}")
    print("\nГотово.")


if __name__ == "__main__":
    main()
