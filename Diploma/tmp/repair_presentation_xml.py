from __future__ import annotations

import re
import shutil
import tempfile
import zipfile
from pathlib import Path


PPTX_PATH = Path("/Users/viktorstehnii/Repositories/Course-4/Diploma/Дипломна.pptx")


def repack_pptx(source_dir: Path, target_path: Path) -> None:
    fd, temp_name = tempfile.mkstemp(suffix=".pptx", prefix="repaired-")
    Path(temp_name).unlink(missing_ok=True)
    temp_pptx = Path(temp_name)
    try:
        with zipfile.ZipFile(temp_pptx, "w", compression=zipfile.ZIP_DEFLATED) as zf:
            for file_path in sorted(source_dir.rglob("*")):
                if file_path.is_file():
                    zf.write(file_path, file_path.relative_to(source_dir).as_posix())
        shutil.move(temp_pptx, target_path)
    finally:
        temp_pptx.unlink(missing_ok=True)


def repair_presentation_xml(xml_text: str) -> str:
    if "<ns0:presentation" not in xml_text:
        return xml_text

    inner = xml_text.split(">", 1)[1].rsplit("</ns0:presentation>", 1)[0]
    inner = inner.replace("ns0:", "p:")
    inner = inner.replace("ns2:", "a:")
    inner = inner.replace("ns3:", "p15:")

    inner = re.sub(r'(<p:sldMasterId id="\d+") id="(rId\d+)"', r'\1 r:id="\2"', inner)
    inner = re.sub(r'(<p:notesMasterId) id="(rId\d+)"', r'\1 r:id="\2"', inner)
    inner = re.sub(r'(<p:sldId id="\d+") id="(rId\d+)"', r'\1 r:id="\2"', inner)

    return (
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
        '<p:presentation '
        'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" '
        'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" '
        'xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main" '
        'xmlns:p15="http://schemas.microsoft.com/office/powerpoint/2012/main" '
        'saveSubsetFonts="1">'
        f"{inner}</p:presentation>"
    )


def main() -> None:
    temp_dir = Path(tempfile.mkdtemp(prefix="pptx-repair-"))
    try:
        with zipfile.ZipFile(PPTX_PATH) as zf:
            zf.extractall(temp_dir)

        pres_path = temp_dir / "ppt/presentation.xml"
        xml_text = pres_path.read_text(encoding="utf-8")
        pres_path.write_text(repair_presentation_xml(xml_text), encoding="utf-8")

        repack_pptx(temp_dir, PPTX_PATH)
    finally:
        shutil.rmtree(temp_dir, ignore_errors=True)


if __name__ == "__main__":
    main()
