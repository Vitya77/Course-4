# Example 1
numbers = [1, 2, 3, 4, 5]

squares = list(map(lambda x: 2 * x, numbers))

print(squares)

# Example 2

def make_multiplier(factor):
    def inner(x):
        return x * factor
    return inner

double = make_multiplier(2)
triple = make_multiplier(3)

print(double(10), triple(10))

# Example 3

students = [
    {"name": "Ivan",  "grade": 90},
    {"name": "Oleh",  "grade": 75},
    {"name": "Maria", "grade": 82},
    {"name": "Sofia", "grade": 60},
]

good_students = list(filter(lambda s: s["grade"] >= 80, students))

print(good_students)

