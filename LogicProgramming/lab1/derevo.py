from graphviz import Digraph

# 🔹 Факти шлюбу (пари)
marriages = [
    ("ivan_1900", "maria_1902"),
    ("maksym_1901", "hanna_1903"),
    ("petro_1925", "kateryna_1930"),
    ("mykola_1926", "olena_1928"),
    ("serhii_1952", "larysa_1953"),
    ("oksana_1955", "volodymyr_1956"),
    ("anatolii_1958", "nataliia_1960"),
    ("andrii_1978", "marina_1980"),
    ("iryna_1981", "pavlo_1979"),
    ("taras_1980", "olha_1982"),
    ("svitlana_1983", "yurii_1982"),
    ("bohdan_1985", "alina_1987"),
]

# 🔹 Факти "батько/мати → дитина"
parents = [
    ("ivan_1900", "petro_1925"), ("maria_1902", "petro_1925"),
    ("ivan_1900", "olena_1928"), ("maria_1902", "olena_1928"),
    ("maksym_1901", "mykola_1926"), ("hanna_1903", "mykola_1926"),
    ("maksym_1901", "kateryna_1930"), ("hanna_1903", "kateryna_1930"),
    ("petro_1925", "serhii_1952"), ("kateryna_1930", "serhii_1952"),
    ("petro_1925", "oksana_1955"), ("kateryna_1930", "oksana_1955"),
    ("petro_1925", "anatolii_1958"), ("kateryna_1930", "anatolii_1958"),
    ("mykola_1926", "larysa_1953"), ("olena_1928", "larysa_1953"),
    ("mykola_1926", "volodymyr_1956"), ("olena_1928", "volodymyr_1956"),
    ("serhii_1952", "andrii_1978"), ("larysa_1953", "andrii_1978"),
    ("serhii_1952", "iryna_1981"), ("larysa_1953", "iryna_1981"),
    ("oksana_1955", "taras_1980"), ("volodymyr_1956", "taras_1980"),
    ("oksana_1955", "svitlana_1983"), ("volodymyr_1956", "svitlana_1983"),
    ("anatolii_1958", "bohdan_1985"), ("nataliia_1960", "bohdan_1985"),
    ("andrii_1978", "marko_2007"), ("marina_1980", "marko_2007"),
    ("iryna_1981", "katia_2012"), ("pavlo_1979", "katia_2012"),
    ("taras_1980", "danylo_2010"), ("olha_1982", "danylo_2010"),
    ("taras_1980", "sofia_2013"), ("olha_1982", "sofia_2013"),
    ("svitlana_1983", "maksym_2015"), ("yurii_1982", "maksym_2015"),
    ("bohdan_1985", "emma_2018"), ("alina_1987", "emma_2018"),
]

# 🔹 Імена кирилицею
names_map = {
    "ivan_1900": "Іван (1900)", "maria_1902": "Марія (1902)",
    "maksym_1901": "Максим (1901)", "hanna_1903": "Ганна (1903)",
    "petro_1925": "Петро (1925)", "kateryna_1930": "Катерина (1930)",
    "mykola_1926": "Микола (1926)", "olena_1928": "Олена (1928)",
    "serhii_1952": "Сергій (1952)", "larysa_1953": "Лариса (1953)",
    "oksana_1955": "Оксана (1955)", "volodymyr_1956": "Володимир (1956)",
    "anatolii_1958": "Анатолій (1958)", "nataliia_1960": "Наталія (1960)",
    "andrii_1978": "Андрій (1978)", "marina_1980": "Марина (1980)",
    "pavlo_1979": "Павло (1979)", "iryna_1981": "Ірина (1981)",
    "taras_1980": "Тарас (1980)", "olha_1982": "Ольга (1982)",
    "yurii_1982": "Юрій (1982)", "svitlana_1983": "Світлана (1983)",
    "bohdan_1985": "Богдан (1985)", "alina_1987": "Аліна (1987)",
    "marko_2007": "Марко (2007)", "katia_2012": "Катя (2012)",
    "danylo_2010": "Данило (2010)", "sofia_2013": "Софія (2013)",
    "maksym_2015": "Максим (2015)", "emma_2018": "Емма (2018)",
}

# 🔹 Будуємо дерево
dot = Digraph("FamilyTree", format="png")
dot.attr(rankdir="TB", size="10")

# Додаємо вузли для кожної людини
for person, label in names_map.items():
    dot.node(person, label, shape="box", style="filled", fillcolor="lightyellow")

# Додаємо "сімейні вузли" для пар
for i, (a, b) in enumerate(marriages):
    fam = f"fam_{i}"
    dot.node(fam, shape="point", width="0.01")  # невидима точка-посередник
    dot.edge(a, fam, dir="none")
    dot.edge(b, fam, dir="none")

    # Додаємо дітей від пари
    for p, c in parents:
        if p in (a, b):
            dot.edge(fam, c)

# Зберігаємо картинку
dot.render("family_tree", format="png", cleanup=True)
print("✅ Сімейне дерево збережено у файлі family_tree.png")