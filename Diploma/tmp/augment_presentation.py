from __future__ import annotations

import copy
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
    "pr": "http://schemas.openxmlformats.org/package/2006/relationships",
    "ep": "http://schemas.openxmlformats.org/officeDocument/2006/extended-properties",
    "vt": "http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes",
    "ct": "http://schemas.openxmlformats.org/package/2006/content-types",
}

for prefix, uri in NS.items():
    ET.register_namespace("" if prefix in {"p", "a", "r"} else prefix, uri)


NEW_SLIDES = [
    {
        "title": "Аутентифікація та навігація",
        "items": [
            "Підтримано кілька сценаріїв входу: email-код, Google, Apple та гостьовий режим.",
            "Пара access/refresh токенів зберігається локально; при 401 клієнт автоматично оновлює access token.",
            "Реалізовано поетапне завершення реєстрації з відновленням незавершеного профілю.",
            "Deep links відкривають профілі, бренди, новини, сповіщення та окремі бажання напряму.",
        ],
    },
    {
        "title": "Push-сповіщення і кешування",
        "items": [
            "На клієнті реалізовано повний цикл Expo Notifications: дозвіл, Expo Push Token, збереження та передача на сервер.",
            "Після натискання на сповіщення користувач переходить одразу до потрібного екрана.",
            "Кешувальний шар на AsyncStorage працює з TTL для окремих сутностей і колекцій.",
            "Після мутацій і push-подій кеш інвалідується, тому інтерфейс лишається швидким і актуальним.",
        ],
    },
    {
        "title": "Аналітика, підтримка та безпека",
        "items": [
            "Firebase Analytics фіксує перегляди екранів, створення бажань, виконання бажань та інші бізнес-події.",
            "Firebase Crashlytics дає змогу моніторити стабільність і причини аварійних завершень.",
            "Канал технічної підтримки поєднує WebSocket для повідомлень і HTTP multipart для вкладень.",
            "Безпека забезпечується режимами видимості бажань, централізованою авторизацією та відкликанням push-токена при logout.",
        ],
    },
    {
        "title": "Монетизація і публікація",
        "items": [
            "Преміум-підписка реалізована через react-native-iap для iOS та Android.",
            "Клієнт підтримує щомісячну й річну моделі доступу та відновлення покупки на іншому пристрої.",
            "Застосунок опубліковано в App Store і Google Play та підтримує обидві мобільні платформи.",
            "Для релізів і оновлень використано Expo, EAS і OTA-механізм expo-updates.",
        ],
    },
    {
        "title": "Новизна та перспективи",
        "items": [
            "Унікальність рішення полягає в поєднанні вішлистів, резервування та копіювання бажань у соціальному середовищі.",
            "Універсальний парсер посилань автоматизує створення бажань з інтернет-магазинів без ручного заповнення.",
            "Наступні кроки розвитку: чати між користувачами, AI-рекомендації та оплата прямо в застосунку.",
            "Подальше масштабування можливе через веб-версію, рейтинги брендів і розширені рекламні сценарії.",
        ],
    },
]


def qn(prefix: str, tag: str) -> str:
    return f"{{{NS[prefix]}}}{tag}"


def read_xml(path: Path) -> ET.ElementTree:
    return ET.parse(path)


def write_xml(path: Path, tree: ET.ElementTree) -> None:
    tree.write(path, encoding="UTF-8", xml_declaration=True)


def find_shape_by_placeholder(root: ET.Element, *, title: bool = False, idx: str | None = None) -> ET.Element:
    for sp in root.findall(".//p:sp", NS):
        ph = sp.find("./p:nvSpPr/p:nvPr/p:ph", NS)
        if ph is None:
            continue
        if title and ph.get("type") == "title":
            return sp
        if idx is not None and ph.get("idx") == idx:
            return sp
    raise ValueError("Required placeholder shape not found")


def set_title_text(title_shape: ET.Element, text: str) -> None:
    tx_body = title_shape.find("./p:txBody", NS)
    if tx_body is None:
        raise ValueError("Title shape has no txBody")
    body_pr = copy.deepcopy(tx_body.find("./a:bodyPr", NS))
    lst_style = copy.deepcopy(tx_body.find("./a:lstStyle", NS))
    first_run = tx_body.find("./a:p/a:r", NS)
    if first_run is None:
        raise ValueError("Title shape has no styled run")
    run_pr = copy.deepcopy(first_run.find("./a:rPr", NS))

    for child in list(tx_body):
        tx_body.remove(child)
    tx_body.append(body_pr)
    tx_body.append(lst_style)

    p = ET.SubElement(tx_body, qn("a", "p"))
    r = ET.SubElement(p, qn("a", "r"))
    r.append(run_pr)
    t = ET.SubElement(r, qn("a", "t"))
    t.text = text


def strip_link_and_underline(run_pr: ET.Element) -> ET.Element:
    cleaned = copy.deepcopy(run_pr)
    for child in list(cleaned):
        if child.tag == qn("a", "hlinkClick"):
            cleaned.remove(child)
    if "u" in cleaned.attrib:
        del cleaned.attrib["u"]
    return cleaned


def set_body_items(body_shape: ET.Element, items: list[str]) -> None:
    tx_body = body_shape.find("./p:txBody", NS)
    if tx_body is None:
        raise ValueError("Body shape has no txBody")
    body_pr = copy.deepcopy(tx_body.find("./a:bodyPr", NS))
    lst_style = copy.deepcopy(tx_body.find("./a:lstStyle", NS))
    template_paragraph = tx_body.find("./a:p", NS)
    if template_paragraph is None:
        raise ValueError("Body shape has no template paragraph")
    template_ppr = copy.deepcopy(template_paragraph.find("./a:pPr", NS))
    template_run = template_paragraph.findall("./a:r", NS)
    if len(template_run) >= 2:
        template_rpr = strip_link_and_underline(template_run[1].find("./a:rPr", NS))
    else:
        template_rpr = strip_link_and_underline(template_run[0].find("./a:rPr", NS))
    end_rpr = copy.deepcopy(template_paragraph.find("./a:endParaRPr", NS))

    for child in list(tx_body):
        tx_body.remove(child)
    tx_body.append(body_pr)
    tx_body.append(lst_style)

    for item in items:
        p = ET.SubElement(tx_body, qn("a", "p"))
        p.append(copy.deepcopy(template_ppr))
        r = ET.SubElement(p, qn("a", "r"))
        r.append(copy.deepcopy(template_rpr))
        t = ET.SubElement(r, qn("a", "t"))
        t.text = item
        p.append(copy.deepcopy(end_rpr))


def make_slide_xml(template_slide: Path, title: str, items: list[str], out_path: Path) -> None:
    tree = read_xml(template_slide)
    root = tree.getroot()
    title_shape = find_shape_by_placeholder(root, title=True)
    body_shape = find_shape_by_placeholder(root, idx="1")
    set_title_text(title_shape, title)
    set_body_items(body_shape, items)
    write_xml(out_path, tree)


def make_slide_rels(template_rels: Path, out_path: Path) -> None:
    tree = read_xml(template_rels)
    root = tree.getroot()
    for rel in list(root):
        if rel.get("Type") != "http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideLayout":
            root.remove(rel)
    write_xml(out_path, tree)


def update_presentation_xml(path: Path, new_rel_ids: list[str]) -> None:
    tree = read_xml(path)
    root = tree.getroot()
    sld_id_lst = root.find("./p:sldIdLst", NS)
    if sld_id_lst is None:
        raise ValueError("Presentation has no slide list")

    thank_you_entry = None
    max_id = 0
    for sld in sld_id_lst.findall("./p:sldId", NS):
        max_id = max(max_id, int(sld.get("id")))
        if sld.get(qn("r", "id")) == "rId33":
            thank_you_entry = sld

    if thank_you_entry is None:
        raise ValueError("Could not find final thank-you slide")

    insert_at = list(sld_id_lst).index(thank_you_entry)
    for offset, rel_id in enumerate(new_rel_ids, start=1):
        new_sld = ET.Element(qn("p", "sldId"))
        new_sld.set("id", str(max_id + offset))
        new_sld.set(qn("r", "id"), rel_id)
        sld_id_lst.insert(insert_at + offset - 1, new_sld)

    write_xml(path, tree)


def update_presentation_rels(path: Path, start_slide_number: int, new_rel_ids: list[str]) -> None:
    tree = read_xml(path)
    root = tree.getroot()
    for idx, rel_id in enumerate(new_rel_ids):
        slide_no = start_slide_number + idx
        rel = ET.Element(qn("pr", "Relationship"))
        rel.set("Id", rel_id)
        rel.set("Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/slide")
        rel.set("Target", f"slides/slide{slide_no}.xml")
        root.append(rel)
    write_xml(path, tree)


def update_content_types(path: Path, start_slide_number: int, slide_count: int) -> None:
    tree = read_xml(path)
    root = tree.getroot()
    for idx in range(slide_count):
        slide_no = start_slide_number + idx
        override = ET.Element(qn("ct", "Override"))
        override.set("PartName", f"/ppt/slides/slide{slide_no}.xml")
        override.set("ContentType", "application/vnd.openxmlformats-officedocument.presentationml.slide+xml")
        root.append(override)
    write_xml(path, tree)


def update_app_props(path: Path, titles: list[str]) -> None:
    tree = read_xml(path)
    root = tree.getroot()

    slides_node = root.find("./ep:Slides", NS)
    if slides_node is not None:
        slides_node.text = str(int(slides_node.text) + len(titles))

    heading_pairs = root.find("./ep:HeadingPairs/vt:vector", NS)
    if heading_pairs is not None:
        variants = heading_pairs.findall("./vt:variant", NS)
        slide_titles_variant = variants[-1].find("./vt:i4", NS)
        if slide_titles_variant is not None:
            slide_titles_variant.text = str(int(slide_titles_variant.text) + len(titles))

    titles_vector = root.find("./ep:TitlesOfParts/vt:vector", NS)
    if titles_vector is not None:
        current_size = int(titles_vector.get("size"))
        titles_vector.set("size", str(current_size + len(titles)))
        parts = titles_vector.findall("./vt:lpstr", NS)
        if not parts:
            raise ValueError("TitlesOfParts is empty")
        last_title = parts[-1]
        insert_at = list(titles_vector).index(last_title)
        for offset, title in enumerate(titles):
            el = ET.Element(qn("vt", "lpstr"))
            el.text = title
            titles_vector.insert(insert_at + offset, el)

    write_xml(path, tree)


def repack_pptx(source_dir: Path, target_path: Path) -> None:
    fd, temp_name = tempfile.mkstemp(suffix=".pptx", prefix="augmented-")
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


def main() -> None:
    temp_dir = Path(tempfile.mkdtemp(prefix="pptx-augment-"))
    try:
        with zipfile.ZipFile(PPTX_PATH) as zf:
            zf.extractall(temp_dir)

        template_slide = temp_dir / "ppt/slides/slide31.xml"
        template_rels = temp_dir / "ppt/slides/_rels/slide31.xml.rels"

        start_slide_number = 33
        new_rel_ids = [f"rId{39 + i}" for i in range(len(NEW_SLIDES))]

        for idx, slide in enumerate(NEW_SLIDES):
            slide_no = start_slide_number + idx
            make_slide_xml(
                template_slide,
                slide["title"],
                slide["items"],
                temp_dir / f"ppt/slides/slide{slide_no}.xml",
            )
            make_slide_rels(
                template_rels,
                temp_dir / f"ppt/slides/_rels/slide{slide_no}.xml.rels",
            )

        update_presentation_xml(temp_dir / "ppt/presentation.xml", new_rel_ids)
        update_presentation_rels(temp_dir / "ppt/_rels/presentation.xml.rels", start_slide_number, new_rel_ids)
        update_content_types(temp_dir / "[Content_Types].xml", start_slide_number, len(NEW_SLIDES))
        update_app_props(temp_dir / "docProps/app.xml", [slide["title"] for slide in NEW_SLIDES])

        repack_pptx(temp_dir, PPTX_PATH)
    finally:
        shutil.rmtree(temp_dir, ignore_errors=True)


if __name__ == "__main__":
    main()
