from __future__ import annotations

import argparse
from pathlib import Path
from typing import Iterable

import cv2
import numpy as np


SUPPORTED_EXTENSIONS = {".png", ".jpg", ".jpeg", ".bmp"}


def find_images(path: Path) -> list[Path]:
    if path.is_file() and path.suffix.lower() in SUPPORTED_EXTENSIONS:
        return [path]
    if path.is_dir():
        return [p for p in sorted(path.rglob("*")) if p.suffix.lower() in SUPPORTED_EXTENSIONS]
    return []


def ensure_bgr(image: np.ndarray) -> np.ndarray:
    if image.ndim == 2:
        return cv2.cvtColor(image, cv2.COLOR_GRAY2BGR)
    return image


def add_title(image: np.ndarray, title: str) -> np.ndarray:
    canvas = ensure_bgr(image).copy()
    cv2.rectangle(canvas, (0, 0), (canvas.shape[1], 30), (255, 255, 255), -1)
    cv2.putText(
        canvas,
        title,
        (10, 20),
        cv2.FONT_HERSHEY_SIMPLEX,
        0.55,
        (0, 0, 0),
        1,
        cv2.LINE_AA,
    )
    return canvas


def resize_to_height(image: np.ndarray, target_h: int) -> np.ndarray:
    h, w = image.shape[:2]
    scale = target_h / max(h, 1)
    target_w = max(int(w * scale), 1)
    return cv2.resize(image, (target_w, target_h), interpolation=cv2.INTER_AREA)


def stack_images(images: Iterable[np.ndarray], target_h: int = 240) -> np.ndarray:
    resized = [resize_to_height(ensure_bgr(image), target_h) for image in images]
    return cv2.hconcat(resized)


def save_outputs(output_dir: Path, stem: str, outputs: dict[str, np.ndarray]) -> None:
    output_dir.mkdir(parents=True, exist_ok=True)
    for name, image in outputs.items():
        cv2.imwrite(str(output_dir / f"{stem}_{name}.png"), image)


def apply_denoising_filters(image_bgr: np.ndarray) -> dict[str, np.ndarray]:
    return {
        "gaussian": cv2.GaussianBlur(image_bgr, (5, 5), 1.2),
        "median": cv2.medianBlur(image_bgr, 5),
        "bilateral": cv2.bilateralFilter(image_bgr, 9, 75, 75),
    }


def apply_sharpening(image_bgr: np.ndarray) -> dict[str, np.ndarray]:
    kernel = np.array([[0, -1, 0], [-1, 5, -1], [0, -1, 0]], dtype=np.float32)
    sharpen_kernel = cv2.filter2D(image_bgr, -1, kernel)

    blurred = cv2.GaussianBlur(image_bgr, (0, 0), 2.0)
    unsharp_mask = cv2.addWeighted(image_bgr, 1.7, blurred, -0.7, 0)

    return {
        "sharpen_kernel": sharpen_kernel,
        "unsharp_mask": unsharp_mask,
    }


def build_binary_masks(gray: np.ndarray) -> dict[str, np.ndarray]:
    _, binary = cv2.threshold(gray, 127, 255, cv2.THRESH_BINARY)
    _, otsu = cv2.threshold(gray, 0, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)
    return {"threshold": binary, "otsu": otsu}


def watershed_segmentation(image_bgr: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
    gray = cv2.cvtColor(image_bgr, cv2.COLOR_BGR2GRAY)
    _, binary_inv = cv2.threshold(gray, 0, 255, cv2.THRESH_BINARY_INV + cv2.THRESH_OTSU)

    kernel = np.ones((3, 3), np.uint8)
    opening = cv2.morphologyEx(binary_inv, cv2.MORPH_OPEN, kernel, iterations=2)
    sure_bg = cv2.dilate(opening, kernel, iterations=3)

    dist_transform = cv2.distanceTransform(opening, cv2.DIST_L2, 5)
    _, sure_fg = cv2.threshold(dist_transform, 0.4 * dist_transform.max(), 255, 0)
    sure_fg = sure_fg.astype(np.uint8)
    unknown = cv2.subtract(sure_bg, sure_fg)

    _, markers = cv2.connectedComponents(sure_fg)
    markers = markers + 1
    markers[unknown == 255] = 0

    watershed_markers = cv2.watershed(image_bgr.copy(), markers)
    overlay = image_bgr.copy()
    overlay[watershed_markers == -1] = (0, 0, 255)

    colored_markers = np.zeros_like(image_bgr)
    positive = watershed_markers > 1
    if np.any(positive):
        normalized = np.zeros_like(watershed_markers, dtype=np.uint8)
        normalized[positive] = np.interp(
            watershed_markers[positive],
            (watershed_markers[positive].min(), watershed_markers[positive].max()),
            (40, 255),
        ).astype(np.uint8)
        colored_markers = cv2.applyColorMap(normalized, cv2.COLORMAP_JET)
    colored_markers[watershed_markers == -1] = (0, 0, 255)
    colored_markers[watershed_markers == 1] = (0, 0, 0)

    return overlay, colored_markers


def grabcut_segmentation(image_bgr: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
    h, w = image_bgr.shape[:2]
    rect = (
        max(w // 10, 1),
        max(h // 10, 1),
        max((w * 8) // 10, 1),
        max((h * 8) // 10, 1),
    )

    mask = np.zeros(image_bgr.shape[:2], np.uint8)
    bg_model = np.zeros((1, 65), np.float64)
    fg_model = np.zeros((1, 65), np.float64)

    cv2.grabCut(image_bgr, mask, rect, bg_model, fg_model, 5, cv2.GC_INIT_WITH_RECT)
    mask_binary = np.where(
        (mask == cv2.GC_FGD) | (mask == cv2.GC_PR_FGD),
        255,
        0,
    ).astype(np.uint8)
    segmented = cv2.bitwise_and(image_bgr, image_bgr, mask=mask_binary)
    return segmented, mask_binary


def process_image(path: Path, output_dir: Path, show: bool) -> None:
    image_bgr = cv2.imread(str(path), cv2.IMREAD_COLOR)
    if image_bgr is None:
        print(f"Не вдалося зчитати: {path}")
        return

    gray = cv2.cvtColor(image_bgr, cv2.COLOR_BGR2GRAY)
    denoised = apply_denoising_filters(image_bgr)
    sharpened = apply_sharpening(image_bgr)
    masks = build_binary_masks(gray)
    watershed_overlay, watershed_regions = watershed_segmentation(image_bgr)
    grabcut_foreground, grabcut_mask = grabcut_segmentation(image_bgr)

    stem_dir = output_dir / path.stem
    outputs = {
        "original": image_bgr,
        "gray": gray,
        **denoised,
        **sharpened,
        "threshold": masks["threshold"],
        "otsu": masks["otsu"],
        "watershed": watershed_overlay,
        "watershed_regions": watershed_regions,
        "grabcut": grabcut_foreground,
        "grabcut_mask": grabcut_mask,
    }
    save_outputs(stem_dir, path.stem, outputs)

    print(f"\n[{path.name}]")
    print(f"  Збережено результатів: {len(outputs)}")
    print(f"  Папка: {stem_dir.resolve()}")

    if show:
        filtering_preview = stack_images(
            [
                add_title(image_bgr, "Original"),
                add_title(denoised["gaussian"], "Gaussian"),
                add_title(denoised["median"], "Median"),
                add_title(denoised["bilateral"], "Bilateral"),
            ],
            target_h=220,
        )
        sharpening_preview = stack_images(
            [
                add_title(image_bgr, "Original"),
                add_title(sharpened["sharpen_kernel"], "Sharpen kernel"),
                add_title(sharpened["unsharp_mask"], "Unsharp mask"),
            ],
            target_h=220,
        )
        segmentation_preview = stack_images(
            [
                add_title(masks["threshold"], "Threshold"),
                add_title(masks["otsu"], "Otsu"),
                add_title(watershed_overlay, "Watershed"),
                add_title(grabcut_foreground, "GrabCut"),
            ],
            target_h=220,
        )

        cv2.imshow(f"{path.name} | Filtration", filtering_preview)
        cv2.imshow(f"{path.name} | Sharpening", sharpening_preview)
        cv2.imshow(f"{path.name} | Segmentation", segmentation_preview)
        key = cv2.waitKey(0) & 0xFF
        cv2.destroyAllWindows()
        if key in (27, ord("q")):
            raise KeyboardInterrupt


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Лабораторна №2: фільтрація, підвищення різкості та сегментація зображень.",
    )
    parser.add_argument("input_path", type=Path, help="Файл або папка із зображеннями.")
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path("lab2_output"),
        help="Папка для збереження результатів.",
    )
    parser.add_argument("--no-show", action="store_true", help="Не відкривати вікна перегляду.")
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    images = find_images(args.input_path)
    if not images:
        print("\nНе знайдено PNG/JPG/BMP зображень.")
        return

    print(f"\nЗнайдено зображень: {len(images)}")
    print(f"Результати буде збережено в: {args.output_dir.resolve()}")

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
