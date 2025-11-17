package main

import (
	"bufio"
	"errors"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
	"sync"
)

const EPS = 1e-12

func gaussJordanParallel(A [][]float64) ([]float64, error) {
	n := len(A)
	if n == 0 {
		return nil, errors.New("пуста матриця")
	}
	for i := 0; i < n; i++ {
		if len(A[i]) != n+1 {
			return nil, fmt.Errorf("рядок %d має довжину %d, очікувалося %d", i, len(A[i]), n+1)
		}
	}

	for col := 0; col < n; col++ {
		pivot := col
		maxAbs := math.Abs(A[col][col])
		for r := col + 1; r < n; r++ {
			if math.Abs(A[r][col]) > maxAbs {
				maxAbs = math.Abs(A[r][col])
				pivot = r
			}
		}
		if maxAbs < EPS {
			return nil, fmt.Errorf("матриця вироджена або близька до виродженої (стовпець %d)", col)
		}

		if pivot != col {
			A[col], A[pivot] = A[pivot], A[col]
		}

		diag := A[col][col]
		for j := col; j <= n; j++ {
			A[col][j] /= diag
		}

		var wg sync.WaitGroup
		for i := 0; i < n; i++ {
			if i == col {
				continue
			}
			wg.Add(1)
			go func(iLocal, colLocal int) {
				defer wg.Done()
				factor := A[iLocal][colLocal]
				if math.Abs(factor) < EPS {
					return
				}
				for j := colLocal; j <= n; j++ {
					A[iLocal][j] -= factor * A[colLocal][j]
				}
				A[iLocal][colLocal] = 0
			}(i, col)
		}
		wg.Wait()
	}

	x := make([]float64, n)
	for i := 0; i < n; i++ {
		x[i] = A[i][n]
	}
	return x, nil
}

func readMatrixFromConsole() [][]float64 {
	reader := bufio.NewReader(os.Stdin)

	fmt.Print("Введіть кількість рівнянь n: ")
	nLine, _ := reader.ReadString('\n')
	nLine = strings.TrimSpace(nLine)
	n, _ := strconv.Atoi(nLine)

	A := make([][]float64, n)
	fmt.Printf("Введіть коефіцієнти (%d рядків по %d чисел):\n", n, n+1)
	for i := 0; i < n; i++ {
		for {
			fmt.Printf("Рядок %d: ", i+1)
			line, _ := reader.ReadString('\n')
			line = strings.TrimSpace(line)
			if line == "" {
				continue
			}
			fields := strings.Fields(line)
			if len(fields) != n+1 {
				fmt.Println("Невірна кількість чисел, спробуйте ще раз.")
				continue
			}
			row := make([]float64, n+1)
			for j := 0; j <= n; j++ {
				val, _ := strconv.ParseFloat(fields[j], 64)
				row[j] = val
			}
			A[i] = row
			break
		}
	}
	return A
}

func main() {
	A := readMatrixFromConsole()

	fmt.Println("\nРозширена матриця:")
	for _, r := range A {
		fmt.Println(r)
	}

	x, err := gaussJordanParallel(A)
	if err != nil {
		fmt.Println("Помилка:", err)
		return
	}

	fmt.Println("\nРозв’язок:")
	for i, val := range x {
		fmt.Printf("x%d = %.6f\n", i+1, val)
	}
}
