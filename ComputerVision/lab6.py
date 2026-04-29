from __future__ import annotations

import argparse
import copy
import os
import random
import textwrap
import urllib.request
import zipfile
from dataclasses import dataclass
from pathlib import Path

import cv2
import numpy as np
import torch
from torch import nn
from torch.utils.data import DataLoader, Dataset
from torchvision import models


BASE_DIR = Path(__file__).resolve().parent
os.environ.setdefault("TORCH_HOME", str(BASE_DIR / ".torch"))

SUPPORTED_EXTENSIONS = {".png", ".jpg", ".jpeg", ".bmp", ".pgm"}
ATT_FACES_URL = "https://www.cl.cam.ac.uk/Research/DTG/attarchive/pub/data/att_faces.zip"
IMAGENET_MEAN = torch.tensor([0.485, 0.456, 0.406], dtype=torch.float32).view(3, 1, 1)
IMAGENET_STD = torch.tensor([0.229, 0.224, 0.225], dtype=torch.float32).view(3, 1, 1)


@dataclass
class BinaryDatasetBundle:
    images: list[np.ndarray]
    labels: np.ndarray


@dataclass
class ExperimentResult:
    name: str
    accuracy: float
    precision: float
    recall: float
    f1: float
    confusion: np.ndarray
    model: nn.Module
    history: list[tuple[float, float]]


class FaceBinaryDataset(Dataset):
    def __init__(self, images: list[np.ndarray], labels: np.ndarray) -> None:
        self.images = images
        self.labels = labels.astype(np.int64)

    def __len__(self) -> int:
        return len(self.images)

    def __getitem__(self, index: int) -> tuple[torch.Tensor, torch.Tensor]:
        image = self.images[index]
        image_rgb = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
        tensor = torch.from_numpy(image_rgb).permute(2, 0, 1).float() / 255.0
        tensor = (tensor - IMAGENET_MEAN) / IMAGENET_STD
        label = torch.tensor(self.labels[index], dtype=torch.long)
        return tensor, label


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Лабораторна №6: детекція облич з pretrained MobileNetV3 і аугментацією датасету.",
    )
    parser.add_argument(
        "--dataset",
        type=Path,
        default=BASE_DIR / "att_faces",
        help="Папка з датасетом AT&T Faces.",
    )
    parser.add_argument(
        "--download-att-faces",
        action="store_true",
        help="Автоматично завантажити AT&T Faces, якщо датасет відсутній.",
    )
    parser.add_argument(
        "--images",
        type=Path,
        default=BASE_DIR / "images",
        help="Папка або файл із зображеннями для детекції.",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=BASE_DIR / "lab6_output",
        help="Папка для збереження моделі, графіки і результатів детекції.",
    )
    parser.add_argument("--image-size", type=int, default=224, help="Розмір вхідного зображення для MobileNet.")
    parser.add_argument("--epochs", type=int, default=5, help="Кількість епох навчання.")
    parser.add_argument("--batch-size", type=int, default=16, help="Розмір batch.")
    parser.add_argument("--lr", type=float, default=1e-3, help="Швидкість навчання.")
    parser.add_argument("--test-size", type=float, default=0.2, help="Частка тестової вибірки.")
    parser.add_argument("--seed", type=int, default=42, help="Зерно випадковості.")
    parser.add_argument("--max-positives", type=int, default=None, help="Обмеження кількості позитивних прикладів.")
    parser.add_argument("--augment-copies", type=int, default=4, help="Скільки аугментованих копій додавати в train.")
    parser.add_argument("--detect-threshold", type=float, default=0.75, help="Поріг імовірності для face box.")
    parser.add_argument(
        "--show",
        action="store_true",
        help="Показати вікна з результатами детекції.",
    )
    parser.add_argument(
        "--no-pretrained",
        action="store_true",
        help="Не використовувати pretrained ваги. Потрібно лише як offline fallback.",
    )
    return parser.parse_args()


def seed_everything(seed: int) -> None:
    random.seed(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)
    if hasattr(torch.backends, "mps"):
        try:
            torch.mps.manual_seed(seed)
        except Exception:
            pass


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
        if not path.name.startswith("s") or not path.name[1:].isdigit():
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


def resolve_device() -> torch.device:
    if torch.cuda.is_available():
        return torch.device("cuda")
    if hasattr(torch.backends, "mps") and torch.backends.mps.is_available():
        return torch.device("mps")
    return torch.device("cpu")


def supported_image_paths(path: Path) -> list[Path]:
    if path.is_file() and path.suffix.lower() in SUPPORTED_EXTENSIONS:
        return [path]
    if path.is_dir():
        return [p for p in sorted(path.rglob("*")) if p.suffix.lower() in SUPPORTED_EXTENSIONS]
    return []


def collect_face_image_paths(dataset_dir: Path, max_positives: int | None) -> list[Path]:
    if not dataset_dir.exists():
        raise FileNotFoundError(f"Папка датасету не знайдена: {dataset_dir}")

    image_paths: list[Path] = []
    for class_dir in sorted(dataset_dir.iterdir()):
        if not class_dir.is_dir():
            continue
        for image_path in sorted(class_dir.iterdir()):
            if image_path.is_file() and image_path.suffix.lower() in SUPPORTED_EXTENSIONS:
                image_paths.append(image_path)

    if max_positives is not None:
        image_paths = image_paths[: max(max_positives, 0)]

    if len(image_paths) < 20:
        raise ValueError("Для лабораторної потрібно хоча б 20 зображень облич.")
    return image_paths


def load_positive_face_images(image_paths: list[Path], image_size: int) -> list[np.ndarray]:
    positives: list[np.ndarray] = []
    for image_path in image_paths:
        gray = cv2.imread(str(image_path), cv2.IMREAD_GRAYSCALE)
        if gray is None:
            continue
        equalized = cv2.equalizeHist(gray)
        bgr = cv2.cvtColor(equalized, cv2.COLOR_GRAY2BGR)
        positives.append(cv2.resize(bgr, (image_size, image_size), interpolation=cv2.INTER_AREA))
    if len(positives) < 20:
        raise ValueError("Не вдалося зчитати достатню кількість позитивних зображень.")
    return positives


def crop_square(image: np.ndarray, x: int, y: int, size: int) -> np.ndarray:
    crop = image[y : y + size, x : x + size]
    return crop.copy()


def random_border_crop(image: np.ndarray, rng: np.random.Generator) -> np.ndarray:
    h, w = image.shape[:2]
    size = max(min(h, w) // 2, 24)
    candidates = [
        (0, 0),
        (max(w - size, 0), 0),
        (0, max(h - size, 0)),
        (max(w - size, 0), max(h - size, 0)),
    ]
    x, y = candidates[int(rng.integers(0, len(candidates)))]
    crop = crop_square(image, x, y, size)
    return cv2.cvtColor(crop, cv2.COLOR_GRAY2BGR)


def intersection_over_union(box_a: tuple[int, int, int, int], box_b: tuple[int, int, int, int]) -> float:
    ax1, ay1, aw, ah = box_a
    bx1, by1, bw, bh = box_b
    ax2, ay2 = ax1 + aw, ay1 + ah
    bx2, by2 = bx1 + bw, by1 + bh

    inter_x1 = max(ax1, bx1)
    inter_y1 = max(ay1, by1)
    inter_x2 = min(ax2, bx2)
    inter_y2 = min(ay2, by2)
    if inter_x2 <= inter_x1 or inter_y2 <= inter_y1:
        return 0.0

    inter_area = float((inter_x2 - inter_x1) * (inter_y2 - inter_y1))
    area_a = float(aw * ah)
    area_b = float(bw * bh)
    return inter_area / max(area_a + area_b - inter_area, 1.0)


def get_frontal_face_detector() -> cv2.CascadeClassifier:
    cascade_path = Path(cv2.data.haarcascades) / "haarcascade_frontalface_default.xml"
    detector = cv2.CascadeClassifier(str(cascade_path))
    if detector.empty():
        raise RuntimeError(f"Не вдалося завантажити Haar Cascade: {cascade_path}")
    return detector


def sample_scene_negative(
    image: np.ndarray,
    face_boxes: list[tuple[int, int, int, int]],
    target_size: int,
    rng: np.random.Generator,
) -> np.ndarray | None:
    h, w = image.shape[:2]
    min_side = min(h, w)
    if min_side < 32:
        return None

    for _ in range(60):
        size = int(rng.integers(max(32, min_side // 6), max(40, min_side // 2)))
        if size >= min_side:
            size = max(min_side - 1, 32)
        if size <= 0 or size > w or size > h:
            continue

        x = int(rng.integers(0, w - size + 1))
        y = int(rng.integers(0, h - size + 1))
        crop_box = (x, y, size, size)
        if any(intersection_over_union(crop_box, face_box) > 0.08 for face_box in face_boxes):
            continue

        crop = crop_square(image, x, y, size)
        if float(np.std(crop)) < 8.0:
            continue
        return cv2.resize(crop, (target_size, target_size), interpolation=cv2.INTER_AREA)

    return None


def synthetic_negative(target_size: int, rng: np.random.Generator) -> np.ndarray:
    pattern = np.zeros((target_size, target_size, 3), dtype=np.uint8)
    mode = int(rng.integers(0, 4))

    if mode == 0:
        noise = rng.normal(128, 50, size=pattern.shape).clip(0, 255)
        pattern[:] = noise.astype(np.uint8)
    elif mode == 1:
        for row in range(target_size):
            value = int(255 * row / max(target_size - 1, 1))
            pattern[row, :, :] = (value, 255 - value, value // 2)
    elif mode == 2:
        step = max(target_size // 8, 8)
        for row in range(0, target_size, step):
            for col in range(0, target_size, step):
                color = 220 if (row // step + col // step) % 2 == 0 else 40
                pattern[row : row + step, col : col + step] = (color, color, color)
    else:
        pattern[:] = int(rng.integers(20, 235))
        for _ in range(18):
            pt1 = (int(rng.integers(0, target_size)), int(rng.integers(0, target_size)))
            pt2 = (int(rng.integers(0, target_size)), int(rng.integers(0, target_size)))
            color = tuple(int(v) for v in rng.integers(0, 255, size=3))
            cv2.line(pattern, pt1, pt2, color, thickness=int(rng.integers(1, 4)), lineType=cv2.LINE_AA)
    return pattern


def create_negative_images(
    face_image_paths: list[Path],
    positives: list[np.ndarray],
    scene_paths: list[Path],
    image_size: int,
    seed: int,
) -> list[np.ndarray]:
    rng = np.random.default_rng(seed)
    negatives: list[np.ndarray] = []
    target_count = len(positives)

    grayscale_faces: list[np.ndarray] = []
    for image_path in face_image_paths:
        gray = cv2.imread(str(image_path), cv2.IMREAD_GRAYSCALE)
        if gray is not None:
            grayscale_faces.append(gray)

    while len(negatives) < target_count // 2 and grayscale_faces:
        source = grayscale_faces[len(negatives) % len(grayscale_faces)]
        crop = random_border_crop(source, rng)
        negatives.append(cv2.resize(crop, (image_size, image_size), interpolation=cv2.INTER_AREA))

    detector = get_frontal_face_detector()
    for scene_path in scene_paths:
        image = cv2.imread(str(scene_path), cv2.IMREAD_COLOR)
        if image is None:
            continue
        gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
        faces = detector.detectMultiScale(gray, scaleFactor=1.08, minNeighbors=5, minSize=(40, 40))
        face_boxes = [tuple(int(v) for v in face) for face in faces]

        for _ in range(120):
            if len(negatives) >= target_count:
                break
            crop = sample_scene_negative(image, face_boxes, image_size, rng)
            if crop is not None:
                negatives.append(crop)
        if len(negatives) >= target_count:
            break

    while len(negatives) < target_count:
        negatives.append(synthetic_negative(image_size, rng))

    return negatives[:target_count]


def build_binary_dataset(
    face_image_paths: list[Path],
    positives: list[np.ndarray],
    scene_paths: list[Path],
    image_size: int,
    seed: int,
) -> BinaryDatasetBundle:
    negatives = create_negative_images(face_image_paths, positives, scene_paths, image_size, seed)
    images = positives + negatives
    labels = np.concatenate(
        [
            np.ones(len(positives), dtype=np.int32),
            np.zeros(len(negatives), dtype=np.int32),
        ]
    )
    return BinaryDatasetBundle(images=images, labels=labels)


def stratified_split(labels: np.ndarray, test_size: float, seed: int) -> tuple[np.ndarray, np.ndarray]:
    rng = np.random.default_rng(seed)
    train_indices: list[int] = []
    test_indices: list[int] = []

    for class_id in np.unique(labels):
        class_indices = np.flatnonzero(labels == class_id)
        shuffled = class_indices.copy()
        rng.shuffle(shuffled)

        test_count = int(round(len(shuffled) * test_size))
        test_count = min(max(test_count, 1), len(shuffled) - 1)
        test_indices.extend(shuffled[:test_count].tolist())
        train_indices.extend(shuffled[test_count:].tolist())

    rng.shuffle(train_indices)
    rng.shuffle(test_indices)
    return np.asarray(train_indices, dtype=np.int32), np.asarray(test_indices, dtype=np.int32)


def rotate_image(image: np.ndarray, angle: float) -> np.ndarray:
    h, w = image.shape[:2]
    matrix = cv2.getRotationMatrix2D((w / 2.0, h / 2.0), angle, 1.0)
    return cv2.warpAffine(image, matrix, (w, h), flags=cv2.INTER_LINEAR, borderMode=cv2.BORDER_REFLECT_101)


def adjust_brightness(image: np.ndarray, factor: float) -> np.ndarray:
    hsv = cv2.cvtColor(image, cv2.COLOR_BGR2HSV).astype(np.float32)
    hsv[:, :, 2] = np.clip(hsv[:, :, 2] * factor, 0, 255)
    return cv2.cvtColor(hsv.astype(np.uint8), cv2.COLOR_HSV2BGR)


def add_gaussian_noise(image: np.ndarray, sigma: float, rng: np.random.Generator) -> np.ndarray:
    noise = rng.normal(0.0, sigma, size=image.shape)
    noisy = image.astype(np.float32) + noise
    return np.clip(noisy, 0, 255).astype(np.uint8)


def apply_augmentations(image: np.ndarray, rng: np.random.Generator, copies: int) -> list[np.ndarray]:
    variants: list[np.ndarray] = []
    for index in range(copies):
        mode = index % 4
        if mode == 0:
            variants.append(rotate_image(image, float(rng.uniform(-18.0, 18.0))))
        elif mode == 1:
            variants.append(adjust_brightness(image, float(rng.uniform(0.7, 1.35))))
        elif mode == 2:
            variants.append(add_gaussian_noise(image, float(rng.uniform(8.0, 22.0)), rng))
        else:
            rotated = rotate_image(image, float(rng.uniform(-12.0, 12.0)))
            bright = adjust_brightness(rotated, float(rng.uniform(0.78, 1.25)))
            variants.append(add_gaussian_noise(bright, float(rng.uniform(6.0, 18.0)), rng))
    return variants


def create_augmented_training_set(
    images: list[np.ndarray],
    labels: np.ndarray,
    seed: int,
    copies: int,
) -> tuple[list[np.ndarray], np.ndarray]:
    rng = np.random.default_rng(seed)
    augmented_images = list(images)
    augmented_labels = labels.astype(np.int32).tolist()

    for image, label in zip(images, labels, strict=True):
        for augmented in apply_augmentations(image, rng, copies):
            augmented_images.append(augmented)
            augmented_labels.append(int(label))

    return augmented_images, np.asarray(augmented_labels, dtype=np.int32)


def build_augmentation_montage(source_image: np.ndarray) -> np.ndarray:
    rng = np.random.default_rng(123)
    variants = [source_image]
    variants.extend(apply_augmentations(source_image, rng, copies=4))
    titles = ["Original", "Rotate", "Brightness", "Noise", "Combined"]

    tiles: list[np.ndarray] = []
    for title, image in zip(titles, variants, strict=True):
        tile = cv2.resize(image, (180, 180), interpolation=cv2.INTER_AREA)
        canvas = np.full((220, 180, 3), 245, dtype=np.uint8)
        canvas[40:220, :, :] = tile
        cv2.putText(canvas, title, (12, 24), cv2.FONT_HERSHEY_SIMPLEX, 0.62, (20, 20, 20), 2, cv2.LINE_AA)
        tiles.append(canvas)
    return cv2.hconcat(tiles)


def build_model(pretrained: bool) -> nn.Module:
    try:
        weights = models.MobileNet_V3_Small_Weights.DEFAULT if pretrained else None
        model = models.mobilenet_v3_small(weights=weights)
    except Exception as exc:
        if pretrained:
            message = (
                "Не вдалося завантажити pretrained ваги MobileNetV3. "
                "Перевірте доступ до інтернету або запустіть з --no-pretrained."
            )
            raise RuntimeError(message) from exc
        model = models.mobilenet_v3_small(weights=None)

    for parameter in model.features.parameters():
        parameter.requires_grad = False

    model.classifier[3] = nn.Linear(model.classifier[3].in_features, 2)
    return model


def evaluate_model(model: nn.Module, loader: DataLoader, device: torch.device) -> tuple[float, float, float, float, np.ndarray]:
    model.eval()
    truth_all: list[int] = []
    pred_all: list[int] = []

    with torch.inference_mode():
        for inputs, labels in loader:
            inputs = inputs.to(device)
            labels = labels.to(device)
            logits = model(inputs)
            predictions = torch.argmax(logits, dim=1)
            truth_all.extend(labels.cpu().numpy().tolist())
            pred_all.extend(predictions.cpu().numpy().tolist())

    y_true = np.asarray(truth_all, dtype=np.int32)
    y_pred = np.asarray(pred_all, dtype=np.int32)

    accuracy = float(np.mean(y_true == y_pred))
    tp = int(np.sum((y_true == 1) & (y_pred == 1)))
    tn = int(np.sum((y_true == 0) & (y_pred == 0)))
    fp = int(np.sum((y_true == 0) & (y_pred == 1)))
    fn = int(np.sum((y_true == 1) & (y_pred == 0)))

    precision = tp / max(tp + fp, 1)
    recall = tp / max(tp + fn, 1)
    f1 = 2 * precision * recall / max(precision + recall, 1e-8)
    confusion = np.array([[tn, fp], [fn, tp]], dtype=np.int32)
    return accuracy, precision, recall, f1, confusion


def train_experiment(
    name: str,
    train_images: list[np.ndarray],
    train_labels: np.ndarray,
    test_images: list[np.ndarray],
    test_labels: np.ndarray,
    args: argparse.Namespace,
    device: torch.device,
) -> ExperimentResult:
    model = build_model(pretrained=not args.no_pretrained).to(device)
    optimizer = torch.optim.AdamW(filter(lambda parameter: parameter.requires_grad, model.parameters()), lr=args.lr)
    criterion = nn.CrossEntropyLoss()

    train_loader = DataLoader(
        FaceBinaryDataset(train_images, train_labels),
        batch_size=args.batch_size,
        shuffle=True,
        num_workers=0,
    )
    test_loader = DataLoader(
        FaceBinaryDataset(test_images, test_labels),
        batch_size=args.batch_size,
        shuffle=False,
        num_workers=0,
    )

    best_state: dict[str, torch.Tensor] | None = None
    best_accuracy = -1.0
    history: list[tuple[float, float]] = []

    for epoch in range(1, args.epochs + 1):
        model.train()
        loss_sum = 0.0
        sample_count = 0

        for inputs, labels in train_loader:
            inputs = inputs.to(device)
            labels = labels.to(device)

            optimizer.zero_grad(set_to_none=True)
            logits = model(inputs)
            loss = criterion(logits, labels)
            loss.backward()
            optimizer.step()

            batch_size = labels.size(0)
            loss_sum += float(loss.item()) * batch_size
            sample_count += batch_size

        train_loss = loss_sum / max(sample_count, 1)
        accuracy, precision, recall, f1, confusion = evaluate_model(model, test_loader, device)
        history.append((train_loss, accuracy))
        print(
            f"[{name}] epoch {epoch:02d}/{args.epochs}: "
            f"loss={train_loss:.4f} acc={accuracy:.4f} precision={precision:.4f} recall={recall:.4f} f1={f1:.4f}"
        )

        if accuracy > best_accuracy:
            best_accuracy = accuracy
            best_state = copy.deepcopy(model.state_dict())

    if best_state is None:
        raise RuntimeError("Не вдалося отримати стан моделі після навчання.")

    model.load_state_dict(best_state)
    accuracy, precision, recall, f1, confusion = evaluate_model(model, test_loader, device)
    return ExperimentResult(
        name=name,
        accuracy=accuracy,
        precision=precision,
        recall=recall,
        f1=f1,
        confusion=confusion,
        model=model,
        history=history,
    )


def save_history_chart(output_path: Path, results: list[ExperimentResult]) -> None:
    canvas = np.full((420, 760, 3), 255, dtype=np.uint8)
    cv2.putText(canvas, "MobileNetV3 Accuracy by Epoch", (24, 34), cv2.FONT_HERSHEY_SIMPLEX, 0.9, (20, 20, 20), 2, cv2.LINE_AA)
    cv2.line(canvas, (70, 360), (710, 360), (50, 50, 50), 2)
    cv2.line(canvas, (70, 80), (70, 360), (50, 50, 50), 2)

    colors = [(30, 90, 220), (30, 170, 30)]
    for grid_idx in range(6):
        y = 360 - int(grid_idx * (280 / 5))
        cv2.line(canvas, (70, y), (710, y), (230, 230, 230), 1)
        cv2.putText(
            canvas,
            f"{grid_idx * 0.2:.1f}",
            (20, y + 4),
            cv2.FONT_HERSHEY_SIMPLEX,
            0.48,
            (80, 80, 80),
            1,
            cv2.LINE_AA,
        )

    max_epochs = max(len(result.history) for result in results)
    for epoch_idx in range(max_epochs):
        x = 70 + int(epoch_idx * 640 / max(max_epochs - 1, 1))
        cv2.putText(canvas, str(epoch_idx + 1), (x - 5, 387), cv2.FONT_HERSHEY_SIMPLEX, 0.48, (70, 70, 70), 1, cv2.LINE_AA)

    for color, result in zip(colors, results, strict=True):
        points: list[tuple[int, int]] = []
        for epoch_idx, (_, accuracy) in enumerate(result.history):
            x = 70 + int(epoch_idx * 640 / max(len(result.history) - 1, 1))
            y = 360 - int(accuracy * 280)
            points.append((x, y))
        for start, end in zip(points, points[1:], strict=False):
            cv2.line(canvas, start, end, color, 3, cv2.LINE_AA)
        for point in points:
            cv2.circle(canvas, point, 4, color, -1)

    legend_y = 54
    for color, result in zip(colors, results, strict=True):
        cv2.rectangle(canvas, (500, legend_y - 10), (522, legend_y + 10), color, -1)
        cv2.putText(canvas, result.name, (532, legend_y + 5), cv2.FONT_HERSHEY_SIMPLEX, 0.55, (30, 30, 30), 1, cv2.LINE_AA)
        legend_y += 28

    cv2.imwrite(str(output_path), canvas)


def save_metrics_report(output_path: Path, dataset: BinaryDatasetBundle, train_count: int, test_count: int, results: list[ExperimentResult]) -> None:
    lines = [
        "Лабораторна робота №6",
        "Тема: pretrained MobileNetV3 для детекції облич + аугментація датасету",
        "",
        f"Усього зображень у бінарному датасеті: {len(dataset.images)}",
        f"Позитивні приклади (обличчя): {int(np.sum(dataset.labels == 1))}",
        f"Негативні приклади: {int(np.sum(dataset.labels == 0))}",
        f"Train: {train_count}",
        f"Test: {test_count}",
        "",
    ]

    for result in results:
        lines.extend(
            [
                f"Експеримент: {result.name}",
                f"  accuracy = {result.accuracy:.4f}",
                f"  precision = {result.precision:.4f}",
                f"  recall = {result.recall:.4f}",
                f"  f1 = {result.f1:.4f}",
                f"  confusion = {result.confusion.tolist()}",
                "",
            ]
        )

    best = max(results, key=lambda item: item.accuracy)
    baseline = next(result for result in results if result.name == "Without augmentation")
    augmented = next(result for result in results if result.name == "With augmentation")
    improvement = augmented.accuracy - baseline.accuracy
    lines.extend(
        [
            f"Найкраща модель: {best.name}",
            f"Покращення accuracy від аугментації: {improvement:.4f}",
        ]
    )
    output_path.write_text("\n".join(lines), encoding="utf-8")


def save_model(output_path: Path, result: ExperimentResult) -> None:
    torch.save(
        {
            "model_state": result.model.state_dict(),
            "accuracy": result.accuracy,
            "precision": result.precision,
            "recall": result.recall,
            "f1": result.f1,
        },
        output_path,
    )


def filter_boxes_nms(boxes: list[tuple[int, int, int, int]], scores: list[float], iou_threshold: float) -> list[int]:
    if not boxes:
        return []

    order = np.argsort(np.asarray(scores))[::-1]
    keep: list[int] = []

    while order.size > 0:
        current = int(order[0])
        keep.append(current)
        remaining: list[int] = []
        for idx in order[1:]:
            if intersection_over_union(boxes[current], boxes[int(idx)]) < iou_threshold:
                remaining.append(int(idx))
        order = np.asarray(remaining, dtype=np.int32)

    return keep


def generate_candidate_boxes(image: np.ndarray) -> list[tuple[int, int, int, int]]:
    gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    frontal = get_frontal_face_detector()
    profile_path = Path(cv2.data.haarcascades) / "haarcascade_profileface.xml"
    profile = cv2.CascadeClassifier(str(profile_path))

    boxes: list[tuple[int, int, int, int]] = []
    frontal_boxes = frontal.detectMultiScale(gray, scaleFactor=1.08, minNeighbors=5, minSize=(40, 40))
    boxes.extend(tuple(int(v) for v in face) for face in frontal_boxes)

    if not profile.empty():
        profile_boxes = profile.detectMultiScale(gray, scaleFactor=1.08, minNeighbors=5, minSize=(40, 40))
        boxes.extend(tuple(int(v) for v in face) for face in profile_boxes)

        flipped = cv2.flip(gray, 1)
        profile_boxes_flipped = profile.detectMultiScale(flipped, scaleFactor=1.08, minNeighbors=5, minSize=(40, 40))
        width = gray.shape[1]
        for x, y, w, h in profile_boxes_flipped:
            mirrored_x = width - x - w
            boxes.append((int(mirrored_x), int(y), int(w), int(h)))

    return boxes


def sliding_window_boxes(image: np.ndarray) -> list[tuple[int, int, int, int]]:
    h, w = image.shape[:2]
    min_side = min(h, w)
    scales = [0.18, 0.24, 0.32, 0.42, 0.52]
    boxes: list[tuple[int, int, int, int]] = []

    for scale in scales:
        size = int(min_side * scale)
        if size < 48 or size > min_side:
            continue
        step = max(size // 5, 24)
        for y in range(0, max(h - size + 1, 1), step):
            for x in range(0, max(w - size + 1, 1), step):
                boxes.append((x, y, size, size))
    return boxes


def predict_face_probabilities(
    model: nn.Module,
    image: np.ndarray,
    boxes: list[tuple[int, int, int, int]],
    image_size: int,
    device: torch.device,
    batch_size: int = 32,
) -> list[float]:
    if not boxes:
        return []

    model.eval()
    probabilities: list[float] = []

    with torch.inference_mode():
        for start in range(0, len(boxes), batch_size):
            batch_boxes = boxes[start : start + batch_size]
            batch_tensors: list[torch.Tensor] = []
            for x, y, w, h in batch_boxes:
                crop = image[y : y + h, x : x + w]
                if crop.size == 0:
                    crop = np.zeros((image_size, image_size, 3), dtype=np.uint8)
                crop = cv2.resize(crop, (image_size, image_size), interpolation=cv2.INTER_AREA)
                crop_rgb = cv2.cvtColor(crop, cv2.COLOR_BGR2RGB)
                tensor = torch.from_numpy(crop_rgb).permute(2, 0, 1).float() / 255.0
                tensor = (tensor - IMAGENET_MEAN) / IMAGENET_STD
                batch_tensors.append(tensor)

            inputs = torch.stack(batch_tensors).to(device)
            probs = torch.softmax(model(inputs), dim=1)[:, 1]
            probabilities.extend(probs.cpu().numpy().astype(float).tolist())

    return probabilities


def detect_faces_with_model(
    image: np.ndarray,
    model: nn.Module,
    image_size: int,
    threshold: float,
    device: torch.device,
) -> list[tuple[tuple[int, int, int, int], float]]:
    candidate_boxes = generate_candidate_boxes(image)
    candidate_scores = predict_face_probabilities(model, image, candidate_boxes, image_size, device)

    accepted_boxes: list[tuple[int, int, int, int]] = []
    accepted_scores: list[float] = []
    proposal_threshold = max(0.55, threshold - 0.12)
    for box, score in zip(candidate_boxes, candidate_scores, strict=True):
        if score >= proposal_threshold:
            accepted_boxes.append(box)
            accepted_scores.append(score)

    if candidate_boxes and not accepted_boxes and candidate_scores:
        best_index = int(np.argmax(np.asarray(candidate_scores)))
        if candidate_scores[best_index] >= max(0.7, threshold):
            accepted_boxes.append(candidate_boxes[best_index])
            accepted_scores.append(candidate_scores[best_index])

    if not candidate_boxes:
        scan_boxes = sliding_window_boxes(image)
        scan_scores = predict_face_probabilities(model, image, scan_boxes, image_size, device)
        for box, score in zip(scan_boxes, scan_scores, strict=True):
            if score >= max(threshold, 0.92):
                accepted_boxes.append(box)
                accepted_scores.append(score)

    keep_indices = filter_boxes_nms(accepted_boxes, accepted_scores, iou_threshold=0.3)
    detections = [(accepted_boxes[index], accepted_scores[index]) for index in keep_indices]
    detections.sort(key=lambda item: item[1], reverse=True)
    return detections[:3]


def annotate_detection(image: np.ndarray, detections: list[tuple[tuple[int, int, int, int], float]], title: str) -> np.ndarray:
    canvas = image.copy()
    cv2.rectangle(canvas, (0, 0), (canvas.shape[1], 36), (255, 255, 255), -1)
    cv2.putText(canvas, title, (10, 24), cv2.FONT_HERSHEY_SIMPLEX, 0.64, (20, 20, 20), 2, cv2.LINE_AA)

    for (x, y, w, h), score in detections:
        cv2.rectangle(canvas, (x, y), (x + w, y + h), (25, 190, 25), 3)
        cv2.putText(
            canvas,
            f"Face {score * 100:.1f}%",
            (x, max(46, y - 10)),
            cv2.FONT_HERSHEY_SIMPLEX,
            0.6,
            (20, 120, 20),
            2,
            cv2.LINE_AA,
        )

    if not detections:
        cv2.putText(canvas, "Faces not found", (10, canvas.shape[0] - 16), cv2.FONT_HERSHEY_SIMPLEX, 0.75, (0, 0, 180), 2, cv2.LINE_AA)
    return canvas


def write_detection_summary(output_path: Path, image_name: str, detections: list[tuple[tuple[int, int, int, int], float]]) -> None:
    lines = [f"Image: {image_name}", f"Detected faces: {len(detections)}"]
    for index, ((x, y, w, h), score) in enumerate(detections, start=1):
        lines.append(f"{index}. box=({x}, {y}, {w}, {h}) probability={score:.4f}")
    output_path.write_text("\n".join(lines), encoding="utf-8")


def run_detection(
    image_paths: list[Path],
    model: nn.Module,
    image_size: int,
    threshold: float,
    device: torch.device,
    output_dir: Path,
    show: bool,
) -> None:
    for image_path in image_paths:
        image = cv2.imread(str(image_path), cv2.IMREAD_COLOR)
        if image is None:
            print(f"Не вдалося зчитати {image_path}")
            continue

        detections = detect_faces_with_model(image, model, image_size, threshold, device)
        annotated = annotate_detection(image, detections, f"MobileNetV3 face detection: {image_path.name}")
        target_image = output_dir / f"{image_path.stem}_detection.png"
        target_txt = output_dir / f"{image_path.stem}_detection.txt"
        cv2.imwrite(str(target_image), annotated)
        write_detection_summary(target_txt, image_path.name, detections)
        print(f"Збережено результат детекції: {target_image}")

        if show:
            cv2.imshow("Lab 6 detection", annotated)
            cv2.waitKey(0)

    if show:
        cv2.destroyAllWindows()


def main() -> None:
    args = parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    seed_everything(args.seed)
    torch.set_num_threads(max(1, min(os.cpu_count() or 1, 4)))

    if args.download_att_faces:
        maybe_download_att_faces(args.dataset)

    device = resolve_device()
    print(f"Використовується пристрій: {device}")

    face_image_paths = collect_face_image_paths(args.dataset, args.max_positives)
    positives = load_positive_face_images(face_image_paths, args.image_size)
    scene_paths = supported_image_paths(args.images)
    dataset = build_binary_dataset(face_image_paths, positives, scene_paths, args.image_size, args.seed)

    train_idx, test_idx = stratified_split(dataset.labels, args.test_size, args.seed)
    train_images = [dataset.images[int(idx)] for idx in train_idx]
    train_labels = dataset.labels[train_idx]
    test_images = [dataset.images[int(idx)] for idx in test_idx]
    test_labels = dataset.labels[test_idx]

    augmented_train_images, augmented_train_labels = create_augmented_training_set(
        train_images,
        train_labels,
        seed=args.seed,
        copies=args.augment_copies,
    )

    montage = build_augmentation_montage(positives[0])
    cv2.imwrite(str(args.output_dir / "augmentation_examples.png"), montage)

    baseline = train_experiment(
        name="Without augmentation",
        train_images=train_images,
        train_labels=train_labels,
        test_images=test_images,
        test_labels=test_labels,
        args=args,
        device=device,
    )
    augmented = train_experiment(
        name="With augmentation",
        train_images=augmented_train_images,
        train_labels=augmented_train_labels,
        test_images=test_images,
        test_labels=test_labels,
        args=args,
        device=device,
    )

    results = [baseline, augmented]
    save_history_chart(args.output_dir / "accuracy_history.png", results)
    save_metrics_report(args.output_dir / "report.txt", dataset, len(train_images), len(test_images), results)
    save_model(args.output_dir / "mobilenet_without_augmentation.pt", baseline)
    save_model(args.output_dir / "mobilenet_with_augmentation.pt", augmented)

    best_result = max(results, key=lambda item: item.accuracy)
    print(f"\nНайкраща модель: {best_result.name} (accuracy={best_result.accuracy:.4f})")

    image_paths = supported_image_paths(args.images)
    if image_paths:
        run_detection(
            image_paths=image_paths,
            model=best_result.model,
            image_size=args.image_size,
            threshold=args.detect_threshold,
            device=device,
            output_dir=args.output_dir,
            show=args.show,
        )
    else:
        print("Зображення для детекції не знайдено, етап inference пропущено.")

    summary = textwrap.dedent(
        f"""
        Підсумок:
        - Датасет побудований з {len(positives)} face і {len(positives)} non-face прикладів.
        - Порівняно дві конфігурації: без аугментації та з аугментацією.
        - Використано pretrained MobileNetV3 Small для бінарної класифікації face/non-face.
        - Найкраща модель: {best_result.name} з accuracy={best_result.accuracy:.4f}.
        """
    ).strip()
    print(f"\n{summary}")


if __name__ == "__main__":
    main()
