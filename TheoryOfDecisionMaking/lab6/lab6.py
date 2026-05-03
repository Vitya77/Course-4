import numpy as np
from itertools import product


# ============================================================
# ВАРІАНТ 15
# y_i[q0] - Phi_i(x1, x2[q1], x3[q3]) = 0
#
# Коригуються: y, x1
# Незмінні: x2, x3
# ============================================================


# ------------------------------------------------------------
# 1. ТУТ ЗАДАЙ СВОЇ ФУНКЦІЇ Phi_i
# ------------------------------------------------------------
LAMBDA_X1 = np.array([
    [0.42996748144588487, 27.467375239818058, -50.83590829955191, 22.920758237164176],
    [0.42996748148278013, 2.4472592261621546, -3.7437771031437697, 1.1189586160747604],
], dtype=float)

LAMBDA_X2 = np.array([
    [0.429967481483107, -0.07254949093289263, -0.09022683995325365, -0.10785523713224429],
    [0.42996748148664016, -1.0441527269220503, 0.5607171481778931, -0.4706446239353391],
], dtype=float)

LAMBDA_X3 = np.array([
    [0.42996748148482117, 0.5946757578242857, -0.36853811329235703, -0.5311362136205051],
    [0.4299674814845725, 0.25641243940230257, -0.0770903538198494, 0.017182885164789087],
    [0.4299674814845725, 0.0017499806715064077, -0.37253629731596644, -0.1373566041437031],
], dtype=float)

A1 = np.array([
    [-1.202485450796301, -0.2911243873012806, -1.0208435373430553, -0.8490486021085494],
    [0.9078473792616664, 1.1192571682539667, 0.3864576648426955, 0.5569274535479593],
], dtype=float)

A2 = np.array([
    [2.4403014829818805, 1.4583468942272686, 0.020196664612934657, -0.04546104241105161],
    [-0.22055590583033033, -0.23033921773037655, -0.9984669125950086, -1.044072557224719],
], dtype=float)

A3 = np.array([
    [0.5825112379857951, 0.7364501123477278, 0.7226937538682633, 0.430854118136766],
    [1.0080215404122659, 0.5755484879744883, 0.6046478302630743, 0.6504619225089889],
    [0.9925219253384887, -0.14852428696371964, 0.7566185084877349, 0.7543057674781599],
], dtype=float)

C_MATRIX = np.array([
    [-0.24075087820986377, -0.6525670571070546, -0.27653572850845143, -0.08757926230549078],
    [0.23049219780124686, 0.37410425713580703, 0.10752831740407148, 0.06616764937550627],
    [1.005281148887618, 1.1925541566049145, 1.1297463806241095, 1.017946768954937],
], dtype=float)

# Масштабування базисних функцій, узяте з навчальної вибірки lab4.
X1_SCALERS = (
    ((0.5, 1.0, True), (0.0, 1.0, False), (-0.5, 1.5, False), (-2.0 / 3.0, 5.0 / 3.0, False)),
    ((0.5, 1.0, True), (0.0, 1.0, False), (-0.5, 1.5, False), (-2.0 / 3.0, 5.0 / 3.0, False)),
)

X2_SCALERS = (
    ((0.5, 1.0, True), (0.0, 2.0, False), (-2.0, 4.0, False), (-5.656312972635005, 5.656312972635005, False)),
    ((0.5, 1.0, True), (0.0, 2.0, False), (-2.0, 4.0, False), (-5.032026467801489, 5.032026467801489, False)),
)

X3_SCALERS = (
    ((0.5, 1.0, True), (-1.0, 2.0, False), (-0.497945687262895, 1.4979456872628951, False), (-1.0, 2.0, False)),
    ((0.5, 1.0, True), (-1.0, 2.0, False), (-0.4998746695453679, 1.499874669545368, False), (-1.0, 2.0, False)),
    ((0.5, 1.0, True), (-1.0, 2.0, False), (-0.49999392734677683, 1.4999939273467768, False), (-1.0, 2.0, False)),
)


def safe_log1p(values):
    clipped = np.maximum(values, -0.999999999)
    return np.log1p(clipped)


def laguerre(x, degree):
    if degree == 0:
        return np.ones_like(x)
    if degree == 1:
        return 1.0 - x

    l_prev2 = np.ones_like(x)
    l_prev1 = 1.0 - x
    for n in range(2, degree + 1):
        current = ((2 * n - 1 - x) * l_prev1 - (n - 1) * l_prev2) / n
        l_prev2, l_prev1 = l_prev1, current
    return l_prev1


def hermite(x, degree):
    if degree == 0:
        return np.ones_like(x)
    if degree == 1:
        return 2.0 * x

    h_prev2 = np.ones_like(x)
    h_prev1 = 2.0 * x
    for n in range(2, degree + 1):
        current = 2.0 * x * h_prev1 - 2.0 * (n - 1) * h_prev2
        h_prev2, h_prev1 = h_prev1, current
    return h_prev1


def shifted_legendre(x, degree):
    z = 2.0 * x - 1.0
    if degree == 0:
        return np.ones_like(z)
    if degree == 1:
        return z

    p_prev2 = np.ones_like(z)
    p_prev1 = z
    for n in range(2, degree + 1):
        current = ((2 * n - 1) * z * p_prev1 - (n - 1) * p_prev2) / n
        p_prev2, p_prev1 = p_prev1, current
    return p_prev1


def raw_basis_value(x, degree, family):
    if degree == 0:
        return np.full_like(x, 0.5, dtype=float)
    if family == "laguerre":
        return laguerre(x, degree)
    if family == "hermite":
        return hermite(x, degree)
    if family == "shifted_legendre":
        return shifted_legendre(x, degree)
    raise ValueError(f"Невідомий базис: {family}")


def normalize_basis(values, scaler, degree, family):
    minimum, diff, is_constant = scaler
    values = np.asarray(values, dtype=float)
    if degree == 0 or is_constant:
        return np.full_like(values, 0.5, dtype=float)
    raw = raw_basis_value(values, degree, family)
    return (raw - minimum) / diff


def calculate_log_psi_single(lambda_matrix, values, family, scalers):
    values = np.asarray(values, dtype=float)
    log_psi = np.zeros(values.shape[0], dtype=float)

    for j in range(values.shape[0]):
        for k in range(lambda_matrix.shape[1]):
            basis = normalize_basis(np.array([values[j]], dtype=float), scalers[j][k], k, family)[0]
            log_psi[j] += lambda_matrix[j, k] * safe_log1p(np.array([basis], dtype=float))[0]

    return log_psi


def Phi(x1, x2, x3):
    """
    Повертає вектор значень [Phi_1, Phi_2, ..., Phi_m].

    x1, x2, x3 — це нормовані numpy-масиви розмірів:
    x1 -> (2,), x2 -> (2,), x3 -> (3,)

    Функція використовує коефіцієнти, відновлені в 4-й лабораторній.
    """
    x1 = np.asarray(x1, dtype=float)
    x2 = np.asarray(x2, dtype=float)
    x3 = np.asarray(x3, dtype=float)

    if x1.shape[0] != 2 or x2.shape[0] != 2 or x3.shape[0] != 3:
        raise ValueError("Очікуються розмірності: x1=(2,), x2=(2,), x3=(3,)")

    psi1_log = calculate_log_psi_single(LAMBDA_X1, x1, "laguerre", X1_SCALERS)
    psi2_log = calculate_log_psi_single(LAMBDA_X2, x2, "hermite", X2_SCALERS)
    psi3_log = calculate_log_psi_single(LAMBDA_X3, x3, "shifted_legendre", X3_SCALERS)

    phi1_log = safe_log1p(np.expm1(psi1_log)) @ A1
    phi2_log = safe_log1p(np.expm1(psi2_log)) @ A2
    phi3_log = safe_log1p(np.expm1(psi3_log)) @ A3

    final_log = np.array([
        C_MATRIX[0, i] * phi1_log[i] + C_MATRIX[1, i] * phi2_log[i] + C_MATRIX[2, i] * phi3_log[i]
        for i in range(C_MATRIX.shape[1])
    ], dtype=float)

    return np.expm1(final_log)


# ------------------------------------------------------------
# 2. ПОЧАТКОВІ МЕЖІ
# ------------------------------------------------------------

# Межі для y: [нижня, верхня] для кожного y_i
Y_BOUNDS = np.array([
    [0.0, 1.0],
    [0.0, 1.0],
    [0.0, 1.0],
    [0.0, 1.0],
], dtype=float)

# Межі для x1 — ці межі будуть коригуватися
X1_BOUNDS = np.array([
    [0.0, 2.0],
    [0.0, 2.0],
], dtype=float)

# Межі для x2 — НЕ коригуються у варіанті 15
X2_BOUNDS = np.array([
    [0.0, 2.0],
    [0.0, 2.0],
], dtype=float)

# Межі для x3 — НЕ коригуються у варіанті 15
X3_BOUNDS = np.array([
    [0.0, 1.0],
    [0.0, 1.0],
    [0.0, 1.0],
], dtype=float)


# ------------------------------------------------------------
# 3. НАЛАШТУВАННЯ СІТКИ
# ------------------------------------------------------------

STEP_X1 = 0.05
STEP_X2 = 1.0
STEP_X3 = 1.0

# Наскільки поступово розширювати межі
DELTA_Y_STEP = 0.1
DELTA_X1_STEP = 0.05

MAX_ITERATIONS = 100


# ------------------------------------------------------------
# 4. СЛУЖБОВІ ФУНКЦІЇ
# ------------------------------------------------------------
def make_grid(bounds, step):
    """
    Створює сітку точок для заданих меж.
    Наприклад, якщо bounds = [[0, 1], [2, 3]],
    то поверне всі комбінації x1, x2.
    """
    ranges = [
        np.arange(low, high + step, step)
        for low, high in bounds
    ]

    return np.array(list(product(*ranges)), dtype=float)


def is_inside_bounds(values, bounds):
    """
    Перевіряє, чи значення values потрапляють у межі bounds.
    """
    return np.all(values >= bounds[:, 0]) and np.all(values <= bounds[:, 1])


def dominates(a, b, directions):
    """
    Перевіряє, чи точка a домінує точку b.

    directions:
    1  — критерій треба максимізувати
    -1 — критерій треба мінімізувати
    """
    a = np.array(a)
    b = np.array(b)
    directions = np.array(directions)

    better_or_equal = directions * a >= directions * b
    strictly_better = directions * a > directions * b

    return np.all(better_or_equal) and np.any(strictly_better)


def pareto_front(points, directions):
    """
    Знаходить множину Парето.
    """
    pareto = []

    for i, p in enumerate(points):
        dominated = False

        for j, q in enumerate(points):
            if i != j and dominates(q["phi"], p["phi"], directions):
                dominated = True
                break

        if not dominated:
            pareto.append(p)

    return pareto


# ------------------------------------------------------------
# 5. ГОЛОВНИЙ АЛГОРИТМ ДЛЯ ВАРІАНТУ 15
# ------------------------------------------------------------
def solve_variant_15():
    x2_grid = make_grid(X2_BOUNDS, STEP_X2)
    x3_grid = make_grid(X3_BOUNDS, STEP_X3)

    # Напрямки оптимізації для Phi_i:
    # 1  — максимізувати
    # -1 — мінімізувати
    #
    # Якщо у твоїй роботі всі критерії треба максимізувати,
    # залиш так:
    directions = [1, 1, 1, 1]

    for iteration in range(MAX_ITERATIONS + 1):
        delta_y = iteration * DELTA_Y_STEP
        delta_x1 = iteration * DELTA_X1_STEP

        # За варіантом 15 коригуються y та x1
        corrected_y_bounds = Y_BOUNDS.copy()
        corrected_y_bounds[:, 0] -= delta_y
        corrected_y_bounds[:, 1] += delta_y

        corrected_x1_bounds = X1_BOUNDS.copy()
        corrected_x1_bounds[:, 0] -= delta_x1
        corrected_x1_bounds[:, 1] += delta_x1

        x1_grid = make_grid(corrected_x1_bounds, STEP_X1)

        feasible_points = []

        for x1 in x1_grid:
            for x2 in x2_grid:
                for x3 in x3_grid:
                    phi = Phi(x1, x2, x3)

                    if is_inside_bounds(phi, corrected_y_bounds):
                        feasible_points.append({
                            "x1": x1,
                            "x2": x2,
                            "x3": x3,
                            "phi": phi,
                            "delta_y": delta_y,
                            "delta_x1": delta_x1,
                        })

        if feasible_points:
            pareto = pareto_front(feasible_points, directions)

            print("✅ Розвʼязок знайдено")
            print(f"Ітерація: {iteration}")
            print(f"Δy = {delta_y}")
            print(f"Δx1 = {delta_x1}")
            print(f"Кількість допустимих точок: {len(feasible_points)}")
            print(f"Кількість точок Парето: {len(pareto)}")
            print()

            print("Перші 10 точок множини Парето:")
            for idx, point in enumerate(pareto[:10], start=1):
                print(f"\nТочка {idx}")
                print(f"x1 = {point['x1']}")
                print(f"x2 = {point['x2']}")
                print(f"x3 = {point['x3']}")
                print(f"Phi = {point['phi']}")

            return pareto

    print("❌ Розвʼязок не знайдено")
    print("Спробуй збільшити MAX_ITERATIONS або кроки коригування.")


# ------------------------------------------------------------
# 6. ЗАПУСК
# ------------------------------------------------------------
if __name__ == "__main__":
    solve_variant_15()
