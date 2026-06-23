from __future__ import annotations

import copy
import re
import shutil
import tempfile
import zipfile
from pathlib import Path
import xml.etree.ElementTree as ET


PPTX_PATH = Path("/Users/viktorstehnii/Repositories/Course-4/Diploma/Дипломна.pptx")

NS = {
    "a": "http://schemas.openxmlformats.org/drawingml/2006/main",
    "p": "http://schemas.openxmlformats.org/presentationml/2006/main",
    "r": "http://schemas.openxmlformats.org/officeDocument/2006/relationships",
}


CODE_SLIDES = [
    {
        "title": "Код: deep links",
        "lines": [
            'prefixes: [Linking.createURL("/"),',
            "'https://bazhay.com/deep_link_open/redirect/']",
            'Wish: "wish/:wishId"',
            'Brand: { path: "home/brand/:slug?" }',
            'Profile: { path: "main/profile/:userId?" }',
        ],
    },
    {
        "title": "Код: TTL кеш",
        "lines": [
            "const item = { data, expiry: Date.now() + this.defaultCacheTime };",
            "await AsyncStorage.setItem(key, JSON.stringify(item));",
            "const item = JSON.parse(jsonValue);",
            "if (item.expiry > Date.now()) return item.data as T;",
            "await AsyncStorage.removeItem(key);",
        ],
    },
    {
        "title": "Код: push token",
        "lines": [
            "const { expoPushToken, notification } = usePushNotifications(...);",
            "AsyncStorage.setItem('BAZHAYexpoPushToken', expoPushToken.data);",
            "notificationsService.subscribeToNotifications(",
            "  expoPushToken.data, localization, authContext",
            ");",
        ],
    },
    {
        "title": "Код: premium IAP",
        "lines": [
            "const success = await RNIap.initConnection();",
            "RNIap.getSubscriptions({ skus: [monthId, yearId] });",
            "const purchase = await RNIap.requestSubscription({ sku, ... });",
            "accountService.becomePremiumIOS(...transactionReceipt...);",
            "accountService.becomePremiumAndroid(...purchaseToken...);",
        ],
    },
]


def qn(prefix: str, tag: str) -> str:
    return f"{{{NS[prefix]}}}{tag}"


def get_slide_count(pptx_path: Path) -> int:
    with zipfile.ZipFile(pptx_path) as zf:
        names = zf.namelist()
    return len([n for n in names if re.fullmatch(r"ppt/slides/slide\d+\.xml", n)])


def make_code_slide_xml(template_slide: Path, title: str, lines: list[str], out_path: Path) -> None:
    tree = ET.parse(template_slide)
    root = tree.getroot()

    title_shape = None
    body_shape = None
    for sp in root.findall(".//p:sp", NS):
        ph = sp.find("./p:nvSpPr/p:nvPr/p:ph", NS)
        if ph is None:
            continue
        if ph.get("type") == "title":
            title_shape = sp
        if ph.get("idx") == "1":
            body_shape = sp
    if title_shape is None or body_shape is None:
        raise ValueError("Template slide is missing placeholders")

    title_tx = title_shape.find("./p:txBody", NS)
    first_title_run_pr = copy.deepcopy(title_tx.find("./a:p/a:r/a:rPr", NS))
    body_pr_title = copy.deepcopy(title_tx.find("./a:bodyPr", NS))
    lst_style_title = copy.deepcopy(title_tx.find("./a:lstStyle", NS))
    for child in list(title_tx):
        title_tx.remove(child)
    title_tx.append(body_pr_title)
    title_tx.append(lst_style_title)
    title_p = ET.SubElement(title_tx, qn("a", "p"))
    title_r = ET.SubElement(title_p, qn("a", "r"))
    title_r.append(first_title_run_pr)
    ET.SubElement(title_r, qn("a", "t")).text = title

    body_tx = body_shape.find("./p:txBody", NS)
    body_pr = copy.deepcopy(body_tx.find("./a:bodyPr", NS))
    lst_style = copy.deepcopy(body_tx.find("./a:lstStyle", NS))
    template_p = body_tx.find("./a:p", NS)
    template_end = copy.deepcopy(template_p.find("./a:endParaRPr", NS))
    template_ppr = copy.deepcopy(template_p.find("./a:pPr", NS))
    for tag in ["buFont", "buAutoNum"]:
        child = template_ppr.find(f"./a:{tag}", NS)
        if child is not None:
            template_ppr.remove(child)
    template_ppr.set("marL", "0")
    template_ppr.set("indent", "0")

    first_body_run = template_p.findall("./a:r", NS)[1]
    run_pr = copy.deepcopy(first_body_run.find("./a:rPr", NS))
    for child in list(run_pr):
        if child.tag == qn("a", "hlinkClick"):
            run_pr.remove(child)
    run_pr.set("sz", "1200")
    for tag in ["latin", "ea", "cs"]:
        node = run_pr.find(f"./a:{tag}", NS)
        if node is not None:
            node.set("typeface", "Courier New")

    for child in list(body_tx):
        body_tx.remove(child)
    body_tx.append(body_pr)
    body_tx.append(lst_style)

    for line in lines:
        p = ET.SubElement(body_tx, qn("a", "p"))
        p.append(copy.deepcopy(template_ppr))
        r = ET.SubElement(p, qn("a", "r"))
        r.append(copy.deepcopy(run_pr))
        ET.SubElement(r, qn("a", "t")).text = line
        p.append(copy.deepcopy(template_end))

    tree.write(out_path, encoding="UTF-8", xml_declaration=True)


def inject_before_last(text: str, marker: str, insert: str) -> str:
    idx = text.rfind(marker)
    if idx == -1:
        raise ValueError(f"Marker not found: {marker}")
    return text[:idx] + insert + text[idx:]


def repack_pptx(source_dir: Path, target_path: Path) -> None:
    tmp = target_path.with_suffix(".tmp.pptx")
    with zipfile.ZipFile(tmp, "w", compression=zipfile.ZIP_DEFLATED) as zf:
        for path in sorted(source_dir.rglob("*")):
            if path.is_file():
                zf.write(path, path.relative_to(source_dir).as_posix())
    shutil.move(tmp, target_path)


def main() -> None:
    current_slide_count = get_slide_count(PPTX_PATH)
    start_slide = current_slide_count + 1
    start_rel = 39 + (current_slide_count - 32)
    start_sld_id = 256 + current_slide_count

    temp_dir = Path(tempfile.mkdtemp(prefix="pptx-code-"))
    try:
        with zipfile.ZipFile(PPTX_PATH) as zf:
            zf.extractall(temp_dir)

        template_slide = temp_dir / "ppt/slides/slide31.xml"
        template_rels = temp_dir / "ppt/slides/_rels/slide33.xml.rels"

        for offset, slide in enumerate(CODE_SLIDES):
            slide_no = start_slide + offset
            make_code_slide_xml(
                template_slide,
                slide["title"],
                slide["lines"],
                temp_dir / f"ppt/slides/slide{slide_no}.xml",
            )
            shutil.copy(template_rels, temp_dir / f"ppt/slides/_rels/slide{slide_no}.xml.rels")

        presentation_xml = (temp_dir / "ppt/presentation.xml").read_text(encoding="utf-8")
        slide_xml_insert = "".join(
            f'<p:sldId id="{start_sld_id + i}" r:id="rId{start_rel + i}" />'
            for i in range(len(CODE_SLIDES))
        )
        presentation_xml = presentation_xml.replace(
            '<p:sldId id="287" r:id="rId33" />',
            slide_xml_insert + '<p:sldId id="287" r:id="rId33" />',
        )
        (temp_dir / "ppt/presentation.xml").write_text(presentation_xml, encoding="utf-8")

        rels_xml = (temp_dir / "ppt/_rels/presentation.xml.rels").read_text(encoding="utf-8")
        rels_insert = "".join(
            f'<Relationship Id="rId{start_rel + i}" '
            'Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/slide" '
            f'Target="slides/slide{start_slide + i}.xml"/>'
            for i in range(len(CODE_SLIDES))
        )
        rels_xml = inject_before_last(rels_xml, "</Relationships>", rels_insert)
        (temp_dir / "ppt/_rels/presentation.xml.rels").write_text(rels_xml, encoding="utf-8")

        types_xml = (temp_dir / "[Content_Types].xml").read_text(encoding="utf-8")
        types_insert = "".join(
            f'<Override PartName="/ppt/slides/slide{start_slide + i}.xml" '
            'ContentType="application/vnd.openxmlformats-officedocument.presentationml.slide+xml"/>'
            for i in range(len(CODE_SLIDES))
        )
        types_xml = inject_before_last(types_xml, "</Types>", types_insert)
        (temp_dir / "[Content_Types].xml").write_text(types_xml, encoding="utf-8")

        app_xml = (temp_dir / "docProps/app.xml").read_text(encoding="utf-8")
        app_xml = app_xml.replace(f"<ep:Slides>{current_slide_count}</ep:Slides>", f"<ep:Slides>{current_slide_count + len(CODE_SLIDES)}</ep:Slides>")
        app_xml = app_xml.replace(
            f"<vt:i4>{current_slide_count}</vt:i4></vt:variant></vt:vector>",
            f"<vt:i4>{current_slide_count + len(CODE_SLIDES)}</vt:i4></vt:variant></vt:vector>",
            1,
        )
        titles_insert = "".join(f"<vt:lpstr>{slide['title']}</vt:lpstr>" for slide in CODE_SLIDES)
        app_xml = app_xml.replace(
            "<vt:lpstr>Дякую за увагу! Готовий відповісти на Ваші питання.</vt:lpstr>",
            titles_insert + "<vt:lpstr>Дякую за увагу! Готовий відповісти на Ваші питання.</vt:lpstr>",
        )
        app_xml = app_xml.replace(
            f'<vt:vector size="{current_slide_count + 10 - 0 if False else 42}"',
            f'<vt:vector size="{42 + len(CODE_SLIDES)}"',
        )
        (temp_dir / "docProps/app.xml").write_text(app_xml, encoding="utf-8")

        repack_pptx(temp_dir, PPTX_PATH)
    finally:
        shutil.rmtree(temp_dir, ignore_errors=True)


if __name__ == "__main__":
    main()
