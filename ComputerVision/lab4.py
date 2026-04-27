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
    cv2.rectangle(canvas, (0, 0), (canvas.shape[1], 34), (255, 255, 255), -1)
    cv2.putText(canvas, title, (10, 23), cv2.FONT_HERSHEY_SIMPLEX, 0.58, (0, 0, 0), 1, cv2.LINE_AA)
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


def get_face_detector() -> cv2.CascadeClassifier:
    cascade_path = Path(cv2.data.haarcascades) / "haarcascade_frontalface_default.xml"
    detector = cv2.CascadeClassifier(str(cascade_path))
    if detector.empty():
        raise RuntimeError(f"Не вдалося завантажити каскад: {cascade_path}")
    return detector


def detect_faces(gray: np.ndarray) -> np.ndarray:
    detector = get_face_detector()
    return detector.detectMultiScale(gray, scaleFactor=1.1, minNeighbors=5, minSize=(80, 80))


def fallback_face_box(image_shape: tuple[int, ...]) -> tuple[int, int, int, int]:
    h, w = image_shape[:2]
    box_w = max(int(w * 0.55), 1)
    box_h = max(int(h * 0.7), 1)
    x = max((w - box_w) // 2, 0)
    y = max((h - box_h) // 2, 0)
    return x, y, box_w, box_h


def expand_face_box(box: tuple[int, int, int, int], image_shape: tuple[int, ...], margin: float = 0.18) -> tuple[int, int, int, int]:
    x, y, w, h = box
    img_h, img_w = image_shape[:2]
    pad_x = int(w * margin)
    pad_y = int(h * margin)

    x1 = max(x - pad_x, 0)
    y1 = max(y - pad_y, 0)
    x2 = min(x + w + pad_x, img_w)
    y2 = min(y + h + pad_y, img_h)
    return x1, y1, x2, y2


def select_primary_face(image_bgr: np.ndarray, faces: np.ndarray) -> tuple[tuple[int, int, int, int], bool]:
    if len(faces) == 0:
        return fallback_face_box(image_bgr.shape), False

    primary = max(faces, key=lambda face: face[2] * face[3])
    return tuple(int(v) for v in primary), True


def draw_face_boxes(image_bgr: np.ndarray, faces: np.ndarray, primary_box: tuple[int, int, int, int], detected: bool) -> np.ndarray:
    output = image_bgr.copy()

    for x, y, w, h in faces:
        cv2.rectangle(output, (x, y), (x + w, y + h), (40, 210, 40), 2)

    px, py, pw, ph = primary_box
    color = (0, 180, 255) if detected else (0, 0, 255)
    label = "Primary face" if detected else "Fallback ROI"
    cv2.rectangle(output, (px, py), (px + pw, py + ph), color, 3)
    cv2.putText(output, label, (px, max(py - 8, 22)), cv2.FONT_HERSHEY_SIMPLEX, 0.6, color, 2, cv2.LINE_AA)
    return output


def crop_face_roi(image_bgr: np.ndarray, face_box: tuple[int, int, int, int]) -> np.ndarray:
    x1, y1, x2, y2 = expand_face_box(face_box, image_bgr.shape)
    return image_bgr[y1:y2, x1:x2].copy()


def scale_face(face_bgr: np.ndarray, scale: float) -> np.ndarray:
    h, w = face_bgr.shape[:2]
    target_w = max(int(w * scale), 1)
    target_h = max(int(h * scale), 1)
    interpolation = cv2.INTER_CUBIC if scale >= 1.0 else cv2.INTER_AREA
    return cv2.resize(face_bgr, (target_w, target_h), interpolation=interpolation)


def rotate_face(face_bgr: np.ndarray, angle: float) -> np.ndarray:
    h, w = face_bgr.shape[:2]
    center = (w / 2.0, h / 2.0)
    matrix = cv2.getRotationMatrix2D(center, angle, 1.0)

    cos_v = abs(matrix[0, 0])
    sin_v = abs(matrix[0, 1])
    bound_w = int((h * sin_v) + (w * cos_v))
    bound_h = int((h * cos_v) + (w * sin_v))

    matrix[0, 2] += bound_w / 2.0 - center[0]
    matrix[1, 2] += bound_h / 2.0 - center[1]

    return cv2.warpAffine(
        face_bgr,
        matrix,
        (bound_w, bound_h),
        flags=cv2.INTER_LINEAR,
        borderMode=cv2.BORDER_REPLICATE,
    )


def perspective_correct_face(face_bgr: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
    h, w = face_bgr.shape[:2]
    src_points = np.float32(
        [
            [0.18 * w, 0.08 * h],
            [0.82 * w, 0.12 * h],
            [0.92 * w, 0.95 * h],
            [0.08 * w, 0.90 * h],
        ]
    )
    dst_points = np.float32(
        [
            [0, 0],
            [w - 1, 0],
            [w - 1, h - 1],
            [0, h - 1],
        ]
    )

    matrix = cv2.getPerspectiveTransform(src_points, dst_points)
    corrected = cv2.warpPerspective(
        face_bgr,
        matrix,
        (w, h),
        flags=cv2.INTER_LINEAR,
        borderMode=cv2.BORDER_REPLICATE,
    )

    annotated = face_bgr.copy()
    points_i32 = src_points.astype(np.int32)
    cv2.polylines(annotated, [points_i32], True, (0, 180, 255), 2)
    for px, py in points_i32:
        cv2.circle(annotated, (int(px), int(py)), 5, (0, 0, 255), -1)

    return annotated, corrected


def keep_main_component(mask: np.ndarray) -> np.ndarray:
    num_labels, labels, stats, centroids = cv2.connectedComponentsWithStats(mask, connectivity=8)
    if num_labels <= 1:
        return mask

    h, w = mask.shape[:2]
    center = np.array([w / 2.0, h / 2.0], dtype=np.float32)
    best_label = 1
    best_score = -1.0

    for label in range(1, num_labels):
        area = int(stats[label, cv2.CC_STAT_AREA])
        if area < max(int(mask.size * 0.01), 40):
            continue

        centroid = centroids[label]
        distance = float(np.linalg.norm(centroid - center))
        score = area / (distance + 1.0)
        if score > best_score:
            best_label = label
            best_score = score

    filtered = np.zeros_like(mask)
    filtered[labels == best_label] = 255
    return filtered


def build_initial_face_mask(face_bgr: np.ndarray) -> np.ndarray:
    ycrcb = cv2.cvtColor(face_bgr, cv2.COLOR_BGR2YCrCb)
    skin_mask = cv2.inRange(ycrcb, (0, 133, 77), (255, 173, 127))
    skin_mask = cv2.medianBlur(skin_mask, 5)

    coverage = float(np.count_nonzero(skin_mask)) / float(skin_mask.size)
    if 0.03 <= coverage <= 0.85:
        return keep_main_component(skin_mask)

    gray = cv2.cvtColor(face_bgr, cv2.COLOR_BGR2GRAY)
    blurred = cv2.GaussianBlur(gray, (5, 5), 0)
    _, otsu = cv2.threshold(blurred, 0, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)

    left_mean = float(np.mean(gray[otsu == 255])) if np.any(otsu == 255) else 0.0
    right_mean = float(np.mean(gray[otsu == 0])) if np.any(otsu == 0) else 255.0
    if left_mean < right_mean:
        otsu = cv2.bitwise_not(otsu)

    return keep_main_component(otsu)


def apply_morphology(mask: np.ndarray) -> dict[str, np.ndarray]:
    kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (5, 5))
    eroded = cv2.erode(mask, kernel, iterations=1)
    dilated = cv2.dilate(mask, kernel, iterations=1)
    opened = cv2.morphologyEx(mask, cv2.MORPH_OPEN, kernel, iterations=1)
    closed = cv2.morphologyEx(mask, cv2.MORPH_CLOSE, kernel, iterations=2)
    cleaned = cv2.morphologyEx(opened, cv2.MORPH_CLOSE, kernel, iterations=2)

    return {
        "mask_initial": mask,
        "mask_eroded": eroded,
        "mask_dilated": dilated,
        "mask_opened": opened,
        "mask_closed": closed,
        "mask_cleaned": cleaned,
    }


def apply_mask(image_bgr: np.ndarray, mask: np.ndarray) -> np.ndarray:
    return cv2.bitwise_and(image_bgr, image_bgr, mask=mask)


def process_image(path: Path, output_dir: Path, show: bool) -> None:
    image_bgr = cv2.imread(str(path), cv2.IMREAD_COLOR)
    if image_bgr is None:
        print(f"Не вдалося зчитати: {path}")
        return

    gray = cv2.cvtColor(image_bgr, cv2.COLOR_BGR2GRAY)
    faces = detect_faces(gray)
    primary_face, detected = select_primary_face(image_bgr, faces)

    faces_view = draw_face_boxes(image_bgr, faces, primary_face, detected)
    face_roi = crop_face_roi(image_bgr, primary_face)

    scaled_down = scale_face(face_roi, 0.80)
    scaled_up = scale_face(face_roi, 1.30)
    rotated_left = rotate_face(face_roi, 18)
    rotated_right = rotate_face(face_roi, -18)
    perspective_points, perspective_corrected = perspective_correct_face(face_roi)

    morphology_masks = apply_morphology(build_initial_face_mask(face_roi))
    segmented_initial = apply_mask(face_roi, morphology_masks["mask_initial"])
    segmented_cleaned = apply_mask(face_roi, morphology_masks["mask_cleaned"])

    stem_dir = output_dir / path.stem
    outputs = {
        "original": image_bgr,
        "faces": faces_view,
        "face_roi": face_roi,
        "scaled_080": scaled_down,
        "scaled_130": scaled_up,
        "rotated_left_18": rotated_left,
        "rotated_right_18": rotated_right,
        "perspective_points": perspective_points,
        "perspective_corrected": perspective_corrected,
        **morphology_masks,
        "segmented_initial": segmented_initial,
        "segmented_cleaned": segmented_cleaned,
    }
    save_outputs(stem_dir, path.stem, outputs)

    print(f"\n[{path.name}]")
    print(f"  Знайдено облич: {len(faces)}")
    print(f"  Основний ROI: {'обличчя' if detected else 'центральна область (fallback)'}")
    print(f"  Папка результатів: {stem_dir.resolve()}")

    if show:
        geometry_preview = stack_images(
            [
                add_title(image_bgr, "Original"),
                add_title(faces_view, "Detected faces"),
                add_title(face_roi, "Face ROI"),
                add_title(perspective_points, "Perspective source"),
            ],
            target_h=230,
        )
        transform_preview = stack_images(
            [
                add_title(scaled_down, "Scale 0.80x"),
                add_title(scaled_up, "Scale 1.30x"),
                add_title(rotated_left, "Rotate +18"),
                add_title(perspective_corrected, "Perspective corrected"),
            ],
            target_h=230,
        )
        morphology_preview = stack_images(
            [
                add_title(morphology_masks["mask_initial"], "Initial mask"),
                add_title(morphology_masks["mask_eroded"], "Erosion"),
                add_title(morphology_masks["mask_dilated"], "Dilation"),
                add_title(morphology_masks["mask_opened"], "Opening"),
                add_title(morphology_masks["mask_closed"], "Closing"),
            ],
            target_h=230,
        )
        segmentation_preview = stack_images(
            [
                add_title(face_roi, "ROI"),
                add_title(segmented_initial, "Initial segmentation"),
                add_title(segmented_cleaned, "Cleaned segmentation"),
                add_title(rotated_right, "Rotate -18"),
            ],
            target_h=230,
        )

        cv2.imshow(f"{path.name} | Geometry", geometry_preview)
        cv2.imshow(f"{path.name} | Transformations", transform_preview)
        cv2.imshow(f"{path.name} | Morphology", morphology_preview)
        cv2.imshow(f"{path.name} | Segmentation", segmentation_preview)
        key = cv2.waitKey(0) & 0xFF
        cv2.destroyAllWindows()
        if key in (27, ord("q")):
            raise KeyboardInterrupt


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Лабораторна №4: геометричні перетворення та морфологічні операції для облич.",
    )
    parser.add_argument("input_path", type=Path, help="Файл або папка із зображеннями.")
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path("lab4_output"),
        help="Папка для результатів.",
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
