from __future__ import annotations

import argparse
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable
import cv2
import numpy as np
from PIL import Image


SUPPORTED_EXTENSIONS = {".png", ".jpg", ".jpeg", ".bmp"}


@dataclass
class ImageInfo:
    path: Path
    width: int
    height: int
    channels: int
    dtype: str
    size_bytes: int
    pil_mode: str
    mean_brightness: float

def find_images(path: Path) -> list[Path]:
    if path.is_file() and path.suffix.lower() in SUPPORTED_EXTENSIONS:
        return [path]
    if path.is_dir():
        return [p for p in sorted(path.rglob("*")) if p.suffix.lower() in SUPPORTED_EXTENSIONS]
    
    return []


def collect_info(path: Path, image_bgr: np.ndarray, image_pil: Image.Image) -> ImageInfo:
    h, w = image_bgr.shape[:2]
    channels = image_bgr.shape[2] if image_bgr.ndim == 3 else 1
    gray = cv2.cvtColor(image_bgr, cv2.COLOR_BGR2GRAY)
    return ImageInfo(
        path=path,
        width=w,
        height=h,
        channels=channels,
        dtype=str(image_bgr.dtype),
        size_bytes=path.stat().st_size,
        pil_mode=image_pil.mode,
        mean_brightness=float(np.mean(gray)),
    )


def print_info(info: ImageInfo) -> None:
    ext = info.path.suffix.lower().replace(".", "").upper()
    print(f"\n[{info.path.name}]")
    print(f"  Формат: {ext}")
    print(f"  Роздільна здатність: {info.width}x{info.height}")
    print(f"  Канали: {info.channels}")
    print(f"  Тип даних: {info.dtype}")
    print(f"  Розмір файлу: {info.size_bytes} байт")
    print(f"  PIL mode: {info.pil_mode}")
    print(f"  Середня яскравість: {info.mean_brightness:.2f}")


def build_histogram(gray: np.ndarray) -> np.ndarray:
    hist = cv2.calcHist([gray], [0], None, [256], [0, 256])
    return hist.flatten()


def histogram_to_image(hist: np.ndarray, width: int = 512, height: int = 300) -> np.ndarray:
    canvas = np.full((height, width, 3), 255, dtype=np.uint8)
    margin_left, margin_right, margin_top, margin_bottom = 40, 15, 20, 35
    plot_w = width - margin_left - margin_right
    plot_h = height - margin_top - margin_bottom

    # Axes
    x0, y0 = margin_left, margin_top + plot_h
    cv2.line(canvas, (x0, margin_top), (x0, y0), (0, 0, 0), 1)
    cv2.line(canvas, (x0, y0), (x0 + plot_w, y0), (0, 0, 0), 1)

    hist_max = float(hist.max()) if hist.max() > 0 else 1.0
    bin_w = plot_w / 256.0

    # Bars
    for i, value in enumerate(hist):
        bar_h = int((float(value) / hist_max) * plot_h)
        x1 = int(x0 + i * bin_w)
        x2 = int(x0 + (i + 1) * bin_w)
        cv2.rectangle(canvas, (x1, y0 - bar_h), (max(x2, x1 + 1), y0), (70, 70, 70), -1)

    # X-axis labels
    for tick in (0, 64, 128, 192, 255):
        tx = int(x0 + tick * bin_w)
        cv2.line(canvas, (tx, y0), (tx, y0 + 4), (0, 0, 0), 1)
        cv2.putText(canvas, str(tick), (tx - 10, y0 + 18), cv2.FONT_HERSHEY_SIMPLEX, 0.4, (0, 0, 0), 1)

    cv2.putText(canvas, "Histogram (brightness)", (margin_left, 14), cv2.FONT_HERSHEY_SIMPLEX, 0.45, (0, 0, 0), 1)
    return canvas


def enhance_contrast(gray: np.ndarray) -> dict[str, np.ndarray]:
    normalized = cv2.normalize(gray, None, 0, 255, cv2.NORM_MINMAX)
    equalized = cv2.equalizeHist(gray)
    clahe = cv2.createCLAHE(clipLimit=2.0, tileGridSize=(8, 8)).apply(gray)
    return {"normalized": normalized, "equalized": equalized, "clahe": clahe}


def stack_images(images: Iterable[np.ndarray], target_h: int = 230) -> np.ndarray:
    resized = []
    for image in images:
        if image.ndim == 2:
            image = cv2.cvtColor(image, cv2.COLOR_GRAY2BGR)
        h, w = image.shape[:2]
        scale = target_h / max(h, 1)
        resized.append(cv2.resize(image, (max(int(w * scale), 1), target_h)))
    return cv2.hconcat(resized)


def process_image(path: Path, output_dir: Path, show: bool) -> None:
    image_bgr = cv2.imread(str(path), cv2.IMREAD_COLOR)
    if image_bgr is None:
        print(f"Не вдалося зчитати: {path}")
        return

    image_pil = Image.open(path)
    info = collect_info(path, image_bgr, image_pil)
    print_info(info)

    gray = cv2.cvtColor(image_bgr, cv2.COLOR_BGR2GRAY)
    hist = build_histogram(gray)
    hist_img = histogram_to_image(hist)
    contrast = enhance_contrast(gray)

    output_dir.mkdir(parents=True, exist_ok=True)
    stem = path.stem
    cv2.imwrite(str(output_dir / f"{stem}_hist.png"), hist_img)
    cv2.imwrite(str(output_dir / f"{stem}_normalized.png"), contrast["normalized"])
    cv2.imwrite(str(output_dir / f"{stem}_equalized.png"), contrast["equalized"])
    cv2.imwrite(str(output_dir / f"{stem}_clahe.png"), contrast["clahe"])

    if show:
        preview = stack_images(
            [image_bgr, gray, contrast["equalized"], contrast["clahe"], hist_img],
            target_h=220,
        )
        cv2.imshow(f"{path.name} | ESC/q - next", preview)
        key = cv2.waitKey(0) & 0xFF
        cv2.destroyAllWindows()
        if key in (27, ord("q")):
            raise KeyboardInterrupt


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Лабораторна №1: аналіз зображень.")
    parser.add_argument("input_path", type=Path, help="Файл або папка з PNG/JPG/BMP.")
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path("lab1_output"),
        help="Папка для результатів.",
    )
    parser.add_argument("--no-show", action="store_true", help="Без вікон перегляду.")
    return parser.parse_args()


def main() -> None:
    args = parse_args()

    images = find_images(args.input_path)
    if not images:
        print("\nНе знайдено PNG/JPG/BMP зображень.")
        return

    print(f"\nЗнайдено зображень: {len(images)}")
    print(f"Результати: {args.output_dir.resolve()}")

    try:
        for image_path in images:
            process_image(image_path, args.output_dir, show=not args.no_show)
    except KeyboardInterrupt:
        print("\nПерегляд зупинено.")
    finally:
        cv2.destroyAllWindows()

    print("\nГотово.")


if __name__ == "__main__":
    main()
