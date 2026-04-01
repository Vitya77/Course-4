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
    cv2.rectangle(canvas, (0, 0), (canvas.shape[1], 32), (255, 255, 255), -1)
    cv2.putText(canvas, title, (10, 22), cv2.FONT_HERSHEY_SIMPLEX, 0.6, (0, 0, 0), 1, cv2.LINE_AA)
    return canvas


def resize_to_height(image: np.ndarray, target_h: int) -> np.ndarray:
    h, w = image.shape[:2]
    scale = target_h / max(h, 1)
    target_w = max(int(w * scale), 1)
    return cv2.resize(image, (target_w, target_h), interpolation=cv2.INTER_AREA)


def stack_images(images: Iterable[np.ndarray], target_h: int = 240) -> np.ndarray:
    resized = [resize_to_height(ensure_bgr(image), target_h) for image in images]
    return cv2.hconcat(resized)


def placeholder_image(shape: tuple[int, int, int], title: str, message: str) -> np.ndarray:
    canvas = np.full(shape, 245, dtype=np.uint8)
    cv2.putText(canvas, title, (20, 50), cv2.FONT_HERSHEY_SIMPLEX, 1.0, (30, 30, 30), 2, cv2.LINE_AA)
    cv2.putText(canvas, message, (20, 95), cv2.FONT_HERSHEY_SIMPLEX, 0.7, (50, 50, 180), 2, cv2.LINE_AA)
    return canvas


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
    faces = detector.detectMultiScale(gray, scaleFactor=1.1, minNeighbors=5, minSize=(80, 80))
    return faces


def draw_face_boxes(image_bgr: np.ndarray, faces: np.ndarray) -> np.ndarray:
    output = image_bgr.copy()
    for x, y, w, h in faces:
        cv2.rectangle(output, (x, y), (x + w, y + h), (0, 200, 0), 2)
    return output


def draw_face_contours(image_bgr: np.ndarray, gray: np.ndarray, faces: np.ndarray) -> np.ndarray:
    output = image_bgr.copy()
    for x, y, w, h in faces:
        roi_gray = gray[y : y + h, x : x + w]
        roi_blur = cv2.GaussianBlur(roi_gray, (5, 5), 0)
        edges = cv2.Canny(roi_blur, 60, 140)
        contours, _ = cv2.findContours(edges, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        if contours:
            contours = sorted(contours, key=cv2.contourArea, reverse=True)[:8]
            shifted = [cnt + np.array([[[x, y]]]) for cnt in contours]
            cv2.drawContours(output, shifted, -1, (0, 0, 255), 2)
        cv2.rectangle(output, (x, y), (x + w, y + h), (0, 180, 255), 1)
    return output


def get_sift_detector():
    if hasattr(cv2, "SIFT_create"):
        return cv2.SIFT_create()
    return None


def get_surf_detector():
    if hasattr(cv2, "xfeatures2d") and hasattr(cv2.xfeatures2d, "SURF_create"):
        try:
            return cv2.xfeatures2d.SURF_create(hessianThreshold=400)
        except cv2.error:
            return None
    return None


def draw_keypoints_for_detector(
    image_bgr: np.ndarray,
    gray: np.ndarray,
    faces: np.ndarray,
    detector,
    label: str,
) -> np.ndarray:
    if detector is None:
        return placeholder_image(image_bgr.shape, label, f"{label} not available in current OpenCV")

    output = image_bgr.copy()
    for x, y, w, h in faces:
        roi_gray = gray[y : y + h, x : x + w]
        keypoints, _ = detector.detectAndCompute(roi_gray, None)
        shifted = []
        for kp in keypoints:
            shifted.append(
                cv2.KeyPoint(
                    x=float(kp.pt[0] + x),
                    y=float(kp.pt[1] + y),
                    size=kp.size,
                    angle=kp.angle,
                    response=kp.response,
                    octave=kp.octave,
                    class_id=kp.class_id,
                )
            )
        output = cv2.drawKeypoints(
            output,
            shifted,
            None,
            color=(0, 255, 255),
            flags=cv2.DRAW_MATCHES_FLAGS_DRAW_RICH_KEYPOINTS,
        )
        cv2.rectangle(output, (x, y), (x + w, y + h), (0, 180, 0), 1)
    return output


def compute_hog_visualization(face_roi: np.ndarray) -> np.ndarray:
    h, w = face_roi.shape[:2]
    win_w = max((w // 8) * 8, 16)
    win_h = max((h // 8) * 8, 16)
    resized = cv2.resize(face_roi, (win_w, win_h), interpolation=cv2.INTER_AREA)
    gray = cv2.cvtColor(resized, cv2.COLOR_BGR2GRAY)

    gx = cv2.Sobel(gray, cv2.CV_32F, 1, 0, ksize=1)
    gy = cv2.Sobel(gray, cv2.CV_32F, 0, 1, ksize=1)
    magnitude, angle = cv2.cartToPolar(gx, gy, angleInDegrees=True)

    cell_size = 8
    vis = np.zeros((win_h, win_w, 3), dtype=np.uint8)
    for cy in range(0, win_h, cell_size):
        for cx in range(0, win_w, cell_size):
            cell_mag = magnitude[cy : cy + cell_size, cx : cx + cell_size]
            cell_ang = angle[cy : cy + cell_size, cx : cx + cell_size]
            if cell_mag.size == 0:
                continue
            mean_mag = float(np.mean(cell_mag))
            mean_ang = float(np.mean(cell_ang)) * np.pi / 180.0
            center = (cx + cell_size // 2, cy + cell_size // 2)
            dx = int(np.cos(mean_ang) * mean_mag / 20.0)
            dy = int(np.sin(mean_ang) * mean_mag / 20.0)
            pt1 = (center[0] - dx, center[1] - dy)
            pt2 = (center[0] + dx, center[1] + dy)
            cv2.line(vis, pt1, pt2, (0, 255, 0), 1, cv2.LINE_AA)
    return vis


def build_hog_face_view(image_bgr: np.ndarray, faces: np.ndarray) -> np.ndarray:
    if len(faces) == 0:
        return placeholder_image(image_bgr.shape, "HOG", "Faces not detected")

    x, y, w, h = max(faces, key=lambda face: face[2] * face[3])
    roi = image_bgr[y : y + h, x : x + w]
    vis = compute_hog_visualization(roi)

    hog = cv2.HOGDescriptor(
        _winSize=(vis.shape[1], vis.shape[0]),
        _blockSize=(16, 16),
        _blockStride=(8, 8),
        _cellSize=(8, 8),
        _nbins=9,
    )
    gray = cv2.cvtColor(cv2.resize(roi, (vis.shape[1], vis.shape[0])), cv2.COLOR_BGR2GRAY)
    descriptor = hog.compute(gray)
    descriptor_info = f"HOG length: {0 if descriptor is None else len(descriptor)}"

    canvas = vis.copy()
    cv2.rectangle(canvas, (0, 0), (canvas.shape[1], 32), (255, 255, 255), -1)
    cv2.putText(canvas, descriptor_info, (10, 22), cv2.FONT_HERSHEY_SIMPLEX, 0.55, (0, 0, 0), 1, cv2.LINE_AA)
    return canvas


def process_face_image(path: Path, output_dir: Path, show: bool) -> None:
    image_bgr = cv2.imread(str(path), cv2.IMREAD_COLOR)
    if image_bgr is None:
        print(f"Не вдалося зчитати: {path}")
        return

    gray = cv2.cvtColor(image_bgr, cv2.COLOR_BGR2GRAY)
    faces = detect_faces(gray)

    face_boxes = draw_face_boxes(image_bgr, faces)
    face_contours = draw_face_contours(image_bgr, gray, faces)
    sift_view = draw_keypoints_for_detector(image_bgr, gray, faces, get_sift_detector(), "SIFT")
    surf_view = draw_keypoints_for_detector(image_bgr, gray, faces, get_surf_detector(), "SURF")
    orb_view = draw_keypoints_for_detector(image_bgr, gray, faces, cv2.ORB_create(nfeatures=500), "ORB")
    hog_view = build_hog_face_view(image_bgr, faces)

    stem_dir = output_dir / path.stem
    outputs = {
        "original": image_bgr,
        "faces": face_boxes,
        "face_contours": face_contours,
        "sift": sift_view,
        "surf": surf_view,
        "orb": orb_view,
        "hog": hog_view,
    }
    save_outputs(stem_dir, path.stem, outputs)

    print(f"\n[{path.name}]")
    print(f"  Знайдено облич: {len(faces)}")
    print(f"  Папка результатів: {stem_dir.resolve()}")

    if show:
        preview1 = stack_images(
            [
                add_title(image_bgr, "Original"),
                add_title(face_boxes, "Detected faces"),
                add_title(face_contours, "Face contours"),
            ],
            target_h=230,
        )
        preview2 = stack_images(
            [
                add_title(sift_view, "SIFT"),
                add_title(surf_view, "SURF"),
                add_title(orb_view, "ORB"),
                add_title(hog_view, "HOG"),
            ],
            target_h=230,
        )
        cv2.imshow(f"{path.name} | Face analysis", preview1)
        cv2.imshow(f"{path.name} | Descriptors", preview2)
        key = cv2.waitKey(0) & 0xFF
        cv2.destroyAllWindows()
        if key in (27, ord("q")):
            raise KeyboardInterrupt


def parse_video_source(source: str) -> int | str:
    return int(source) if source.isdigit() else source


def draw_flow_vectors(flow: np.ndarray, frame_bgr: np.ndarray, step: int = 16) -> np.ndarray:
    output = frame_bgr.copy()
    h, w = frame_bgr.shape[:2]
    y_coords, x_coords = np.mgrid[step // 2 : h : step, step // 2 : w : step].astype(int)
    fx, fy = flow[y_coords, x_coords].transpose(2, 0, 1)
    for x, y, dx, dy in zip(x_coords.flatten(), y_coords.flatten(), fx.flatten(), fy.flatten()):
        cv2.arrowedLine(
            output,
            (int(x), int(y)),
            (int(x + dx), int(y + dy)),
            (0, 255, 0),
            1,
            tipLength=0.3,
        )
    return output


def flow_to_color(flow: np.ndarray) -> np.ndarray:
    magnitude, angle = cv2.cartToPolar(flow[..., 0], flow[..., 1], angleInDegrees=True)
    hsv = np.zeros((flow.shape[0], flow.shape[1], 3), dtype=np.uint8)
    hsv[..., 0] = (angle / 2).astype(np.uint8)
    hsv[..., 1] = 255
    hsv[..., 2] = cv2.normalize(magnitude, None, 0, 255, cv2.NORM_MINMAX).astype(np.uint8)
    return cv2.cvtColor(hsv, cv2.COLOR_HSV2BGR)


def extract_foreground(mask: np.ndarray, frame_bgr: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
    kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (5, 5))
    cleaned = cv2.morphologyEx(mask, cv2.MORPH_OPEN, kernel, iterations=1)
    cleaned = cv2.morphologyEx(cleaned, cv2.MORPH_CLOSE, kernel, iterations=2)

    contours, _ = cv2.findContours(cleaned, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    boxed = frame_bgr.copy()
    for contour in contours:
        area = cv2.contourArea(contour)
        if area < 900:
            continue
        x, y, w, h = cv2.boundingRect(contour)
        cv2.rectangle(boxed, (x, y), (x + w, y + h), (0, 180, 255), 2)

    foreground = cv2.bitwise_and(frame_bgr, frame_bgr, mask=cleaned)
    return cleaned, stack_images([boxed, foreground], target_h=240)


def process_video(source: str, output_dir: Path, show: bool, max_frames: int) -> None:
    capture = cv2.VideoCapture(parse_video_source(source))
    if not capture.isOpened():
        print(f"Не вдалося відкрити відеоджерело: {source}")
        return

    output_dir.mkdir(parents=True, exist_ok=True)
    bg_subtractor = cv2.createBackgroundSubtractorMOG2(history=300, varThreshold=40, detectShadows=True)

    ok, prev_frame = capture.read()
    if not ok:
        print("Не вдалося прочитати перший кадр.")
        capture.release()
        return

    prev_gray = cv2.cvtColor(prev_frame, cv2.COLOR_BGR2GRAY)
    frame_idx = 0
    saved = 0

    while True:
        ok, frame = capture.read()
        if not ok:
            break

        frame_idx += 1
        gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)
        flow = cv2.calcOpticalFlowFarneback(
            prev_gray,
            gray,
            None,
            pyr_scale=0.5,
            levels=3,
            winsize=15,
            iterations=3,
            poly_n=5,
            poly_sigma=1.2,
            flags=0,
        )

        flow_vectors = draw_flow_vectors(flow, frame)
        flow_heatmap = flow_to_color(flow)

        fg_mask_raw = bg_subtractor.apply(frame)
        fg_mask, foreground_view = extract_foreground(fg_mask_raw, frame)

        if frame_idx % 30 == 0:
            cv2.imwrite(str(output_dir / f"frame_{frame_idx:04d}_optical_flow.png"), flow_vectors)
            cv2.imwrite(str(output_dir / f"frame_{frame_idx:04d}_flow_heatmap.png"), flow_heatmap)
            cv2.imwrite(str(output_dir / f"frame_{frame_idx:04d}_fg_mask.png"), fg_mask)
            cv2.imwrite(str(output_dir / f"frame_{frame_idx:04d}_foreground.png"), foreground_view)
            saved += 4

        if show:
            optical_flow_preview = stack_images(
                [
                    add_title(frame, "Original"),
                    add_title(flow_vectors, "Optical flow"),
                    add_title(flow_heatmap, "Flow heatmap"),
                ],
                target_h=230,
            )
            subtraction_preview = stack_images(
                [
                    add_title(fg_mask, "Foreground mask"),
                    add_title(foreground_view, "Background subtraction"),
                ],
                target_h=230,
            )
            cv2.imshow("Lab3 | Optical Flow", optical_flow_preview)
            cv2.imshow("Lab3 | Background Subtraction", subtraction_preview)
            key = cv2.waitKey(1) & 0xFF
            if key in (27, ord("q")):
                break

        prev_gray = gray
        if max_frames > 0 and frame_idx >= max_frames:
            break

    capture.release()
    cv2.destroyAllWindows()
    print(f"\nОброблено кадрів: {frame_idx}")
    print(f"Збережено файлів: {saved}")
    print(f"Папка результатів: {output_dir.resolve()}")


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Лабораторна №3: ознаки, дескриптори облич та аналіз відеопотоку.",
    )
    subparsers = parser.add_subparsers(dest="mode", required=True)

    image_parser = subparsers.add_parser("image", help="Аналіз зображень облич.")
    image_parser.add_argument("input_path", type=Path, help="Файл або папка із зображеннями.")
    image_parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path("lab3_output/images"),
        help="Папка для результатів обробки зображень.",
    )
    image_parser.add_argument("--no-show", action="store_true", help="Не відкривати вікна перегляду.")

    video_parser = subparsers.add_parser("video", help="Аналіз відеопотоку або камери.")
    video_parser.add_argument("source", help="Шлях до відео або індекс камери, наприклад 0.")
    video_parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path("lab3_output/video"),
        help="Папка для результатів аналізу відео.",
    )
    video_parser.add_argument("--max-frames", type=int, default=300, help="Максимальна кількість кадрів.")
    video_parser.add_argument("--no-show", action="store_true", help="Не відкривати вікна перегляду.")

    return parser


def main() -> None:
    args = build_parser().parse_args()

    if args.mode == "image":
        images = find_images(args.input_path)
        if not images:
            print("\nНе знайдено PNG/JPG/BMP зображень.")
            return

        print(f"\nЗнайдено зображень: {len(images)}")
        print(f"Результати буде збережено в: {args.output_dir.resolve()}")

        try:
            for image_path in images:
                process_face_image(image_path, args.output_dir, show=not args.no_show)
        except KeyboardInterrupt:
            print("\nПерегляд зупинено.")
        finally:
            cv2.destroyAllWindows()

    if args.mode == "video":
        process_video(args.source, args.output_dir, show=not args.no_show, max_frames=args.max_frames)

    print("\nГотово.")


if __name__ == "__main__":
    main()
