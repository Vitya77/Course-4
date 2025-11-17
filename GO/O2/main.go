package main

import (
	"errors"
	"fmt"
	"math"
	"sort"
)

type Matrix struct {
	rows, cols int
	data       [][]float64
}

func NewMatrix(rows, cols int) *Matrix {
	data := make([][]float64, rows)
	for i := range data {
		data[i] = make([]float64, cols)
	}
	return &Matrix{rows, cols, data}
}

func (m *Matrix) Clone() *Matrix {
	copyData := make([][]float64, m.rows)
	for i := range m.data {
		copyData[i] = append([]float64{}, m.data[i]...)
	}
	return &Matrix{m.rows, m.cols, copyData}
}

func (m *Matrix) Input() {
	fmt.Printf("Введіть елементи матриці (%dx%d):\n", m.rows, m.cols)
	for i := 0; i < m.rows; i++ {
		for j := 0; j < m.cols; j++ {
			fmt.Printf("[%d,%d]: ", i+1, j+1)
			fmt.Scan(&m.data[i][j])
		}
	}
}

func (m *Matrix) Print() {
	for _, row := range m.data {
		for _, val := range row {
			fmt.Printf("%8.3f ", val)
		}
		fmt.Println()
	}
}

func (m *Matrix) Add(n *Matrix) (*Matrix, error) {
	if m.rows != n.rows || m.cols != n.cols {
		return nil, errors.New("неможливо додати: різні розміри")
	}
	res := NewMatrix(m.rows, m.cols)
	for i := 0; i < m.rows; i++ {
		for j := 0; j < m.cols; j++ {
			res.data[i][j] = m.data[i][j] + n.data[i][j]
		}
	}
	return res, nil
}

func (m *Matrix) Sub(n *Matrix) (*Matrix, error) {
	if m.rows != n.rows || m.cols != n.cols {
		return nil, errors.New("неможливо відняти: різні розміри")
	}
	res := NewMatrix(m.rows, m.cols)
	for i := 0; i < m.rows; i++ {
		for j := 0; j < m.cols; j++ {
			res.data[i][j] = m.data[i][j] - n.data[i][j]
		}
	}
	return res, nil
}

func (m *Matrix) Mul(n *Matrix) (*Matrix, error) {
	if m.cols != n.rows {
		return nil, errors.New("неможливо перемножити: несумісні розміри")
	}
	res := NewMatrix(m.rows, n.cols)
	for i := 0; i < m.rows; i++ {
		for j := 0; j < n.cols; j++ {
			for k := 0; k < m.cols; k++ {
				res.data[i][j] += m.data[i][k] * n.data[k][j]
			}
		}
	}
	return res, nil
}

func (m *Matrix) Transpose() *Matrix {
	res := NewMatrix(m.cols, m.rows)
	for i := 0; i < m.rows; i++ {
		for j := 0; j < m.cols; j++ {
			res.data[j][i] = m.data[i][j]
		}
	}
	return res
}

func (m *Matrix) Determinant() (float64, error) {
	if m.rows != m.cols {
		return 0, errors.New("визначник лише для квадратних матриць")
	}

	n := m.rows
	a := m.Clone().data
	det := 1.0

	for i := 0; i < n; i++ {
		pivot := a[i][i]
		if math.Abs(pivot) < 1e-10 {
			found := false
			for j := i + 1; j < n; j++ {
				if math.Abs(a[j][i]) > 1e-10 {
					a[i], a[j] = a[j], a[i]
					det *= -1
					pivot = a[i][i]
					found = true
					break
				}
			}
			if !found {
				return 0, nil
			}
		}

		det *= pivot
		for j := i + 1; j < n; j++ {
			factor := a[j][i] / pivot
			for k := i; k < n; k++ {
				a[j][k] -= factor * a[i][k]
			}
		}
	}

	return det, nil
}

func (m *Matrix) Inverse() (*Matrix, error) {
	if m.rows != m.cols {
		return nil, errors.New("обернена матриця лише для квадратних")
	}

	n := m.rows
	a := m.Clone().data
	inv := NewMatrix(n, n)
	for i := 0; i < n; i++ {
		inv.data[i][i] = 1
	}

	for i := 0; i < n; i++ {
		pivot := a[i][i]
		if math.Abs(pivot) < 1e-10 {
			return nil, errors.New("матриця вироджена, оберненої немає")
		}
		for j := 0; j < n; j++ {
			a[i][j] /= pivot
			inv.data[i][j] /= pivot
		}
		for k := 0; k < n; k++ {
			if k != i {
				factor := a[k][i]
				for j := 0; j < n; j++ {
					a[k][j] -= factor * a[i][j]
					inv.data[k][j] -= factor * inv.data[i][j]
				}
			}
		}
	}

	return inv, nil
}

func (m *Matrix) Solve(b []float64) ([]float64, error) {
	if m.rows != m.cols {
		return nil, errors.New("матриця не квадратна")
	}
	n := m.rows
	a := m.Clone().data
	x := make([]float64, n)

	for i := 0; i < n; i++ {
		pivot := a[i][i]
		if math.Abs(pivot) < 1e-10 {
			return nil, errors.New("матриця вироджена")
		}
		for j := i; j < n; j++ {
			a[i][j] /= pivot
		}
		b[i] /= pivot

		for k := i + 1; k < n; k++ {
			factor := a[k][i]
			for j := i; j < n; j++ {
				a[k][j] -= factor * a[i][j]
			}
			b[k] -= factor * b[i]
		}
	}

	for i := n - 1; i >= 0; i-- {
		x[i] = b[i]
		for j := i + 1; j < n; j++ {
			x[i] -= a[i][j] * x[j]
		}
	}
	return x, nil
}

type RowSortedMatrix struct{ *Matrix }
type ColSortedMatrix struct{ *Matrix }

func (r RowSortedMatrix) Sort() {
	sort.Slice(r.data, func(i, j int) bool {
		for k := 0; k < r.cols; k++ {
			if r.data[i][k] != r.data[j][k] {
				return r.data[i][k] < r.data[j][k]
			}
		}
		return false
	})
}

func (c ColSortedMatrix) Sort() {
	sort.Slice(c.data, func(i, j int) bool {
		for k := 0; k < c.rows; k++ {
			if c.data[k][i] != c.data[k][j] {
				return c.data[k][i] < c.data[k][j]
			}
		}
		return false
	})
}

func main() {
	for {
		fmt.Println("\nМЕНЮ")
		fmt.Println("1. Додати матриці")
		fmt.Println("2. Відняти матриці")
		fmt.Println("3. Множення матриць")
		fmt.Println("4. Транспонування")
		fmt.Println("5. Визначник")
		fmt.Println("6. Обернена матриця")
		fmt.Println("7. Розв’язати СЛАР")
		fmt.Println("8. Сортування рядків лексикографічно")
		fmt.Println("9. Вихід")

		var choice int
		fmt.Print("Оберіть дію: ")
		fmt.Scan(&choice)

		if choice == 9 {
			break
		}

		switch choice {
		case 1, 2, 3:
			var r1, c1, r2, c2 int
			fmt.Print("Розмір матриці A (рядки стовпці): ")
			fmt.Scan(&r1, &c1)
			A := NewMatrix(r1, c1)
			A.Input()

			fmt.Print("Розмір матриці B (рядки стовпці): ")
			fmt.Scan(&r2, &c2)
			B := NewMatrix(r2, c2)
			B.Input()

			switch choice {
			case 1:
				res, err := A.Add(B)
				if err != nil {
					fmt.Println("Помилка:", err)
				} else {
					fmt.Println("Результат додавання:")
					res.Print()
				}
			case 2:
				res, err := A.Sub(B)
				if err != nil {
					fmt.Println("Помилка:", err)
				} else {
					fmt.Println("Результат віднімання:")
					res.Print()
				}
			case 3:
				res, err := A.Mul(B)
				if err != nil {
					fmt.Println("Помилка:", err)
				} else {
					fmt.Println("Результат множення:")
					res.Print()
				}
			}

		case 4:
			var r, c int
			fmt.Print("Розмір матриці: ")
			fmt.Scan(&r, &c)
			M := NewMatrix(r, c)
			M.Input()
			fmt.Println("Транспонована:")
			M.Transpose().Print()

		case 5:
			var n int
			fmt.Print("Розмір квадратної матриці: ")
			fmt.Scan(&n)
			M := NewMatrix(n, n)
			M.Input()
			det, err := M.Determinant()
			if err != nil {
				fmt.Println("Помилка:", err)
			} else {
				fmt.Printf("Визначник = %.3f\n", det)
			}

		case 6:
			var n int
			fmt.Print("Розмір квадратної матриці: ")
			fmt.Scan(&n)
			M := NewMatrix(n, n)
			M.Input()
			inv, err := M.Inverse()
			if err != nil {
				fmt.Println("Помилка:", err)
			} else {
				fmt.Println("Обернена матриця:")
				inv.Print()
			}

		case 7:
			var n int
			fmt.Print("Розмір системи (n): ")
			fmt.Scan(&n)
			A := NewMatrix(n, n)
			A.Input()
			b := make([]float64, n)
			fmt.Println("Введіть вектор b:")
			for i := range b {
				fmt.Printf("b[%d]: ", i+1)
				fmt.Scan(&b[i])
			}
			x, err := A.Solve(b)
			if err != nil {
				fmt.Println("Помилка:", err)
			} else {
				fmt.Println("Розв’язок:")
				for i, v := range x {
					fmt.Printf("x[%d] = %.3f\n", i+1, v)
				}
			}

		case 8:
			var r, c int
			fmt.Print("Розмір матриці: ")
			fmt.Scan(&r, &c)
			M := NewMatrix(r, c)
			M.Input()
			fmt.Print("Сортування за (рядками - 1, стовпцями - 2): ")
			var sortType int
			fmt.Println("Оберіть тип сортування:")
			fmt.Println("1. За рядками")
			fmt.Println("2. За стовпцями")
			fmt.Print("Ваш вибір: ")
			fmt.Scan(&sortType)

			switch sortType {
			case 1:
				rm := RowSortedMatrix{M}
				rm.Sort()
				fmt.Println("Матриця після сортування рядків:")
				rm.Print()
			case 2:
				cm := ColSortedMatrix{M}
				cm.Sort()
				fmt.Println("Матриця після сортування стовпців:")
				cm.Print()
			default:
				fmt.Println("Невірний вибір типу сортування.")
			}
		default:
			fmt.Println("Невірний вибір!")
		}
	}
}
