package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

var in = bufio.NewScanner(os.Stdin)

func readLine() string {
	if !in.Scan() {
		return ""
	}
	return strings.TrimSpace(in.Text())
}

func readInt(prompt string) int {
	for {
		fmt.Print(prompt)
		line := readLine()
		if line == "" {
			fmt.Println("Пустий ввід, спробуйте знову.")
			continue
		}
		x, err := strconv.Atoi(line)
		if err != nil {
			fmt.Println("Не число, спробуйте ще раз.")
			continue
		}
		return x
	}
}

func readWords(prompt string) []string {
	fmt.Print(prompt)
	line := readLine()
	if line == "" {
		return []string{}
	}
	fields := strings.Fields(line)
	return fields
}

func main() {
	in.Split(bufio.ScanLines)
	fmt.Println("=== Побудова орієнтованого графа зі списків вершин ===")

	n := readInt("Кількість вершин у графі: ")
	if n <= 0 {
		fmt.Println("Невірна кількість вершин. Вихід.")
		return
	}

	fmt.Println("Введіть імена вершин (неперервні слова без пробілів). Можна ввести по одному або всі в один рядок.")
	vertices := make([]string, 0, n)
	vertexSet := make(map[string]bool)
	for len(vertices) < n {
		rem := n - len(vertices)
		prompt := fmt.Sprintf("Введіть до %d імен(ни) вершин (через проміжок): ", rem)
		words := readWords(prompt)
		for _, w := range words {
			if w == "" {
				continue
			}
			if vertexSet[w] {
				fmt.Printf("Увага: вершина %q вже існує — ігнорую дубль.\n", w)
				continue
			}
			vertices = append(vertices, w)
			vertexSet[w] = true
			if len(vertices) == n {
				break
			}
		}
	}

	m := readInt("Кількість ребер у графі: ")
	edges := make(map[string]map[string]bool)
	for _, v := range vertices {
		edges[v] = make(map[string]bool)
	}

	fmt.Println("Введіть ребра у форматі: from to (по одній парі в рядку). Приклад: A B")
	for i := 0; i < m; i++ {
		for {
			fmt.Printf("Ребро %d: ", i+1)
			line := readLine()
			parts := strings.Fields(line)
			if len(parts) != 2 {
				fmt.Println("Потрібно два слова: from to. Спробуйте ще.")
				continue
			}
			from, to := parts[0], parts[1]
			if !vertexSet[from] {
				fmt.Printf("Вершина %q відсутня у списку вершин. Спробуйте ще.\n", from)
				continue
			}
			if !vertexSet[to] {
				fmt.Printf("Вершина %q відсутня у списку вершин. Спробуйте ще.\n", to)
				continue
			}
			if edges[from] == nil {
				edges[from] = make(map[string]bool)
			}
			edges[from][to] = true
			break
		}
	}

	k := readInt("Кількість іменованих множин: ")
	sets := make(map[string][]string)
	vertexBelongs := make(map[string]bool)
	for i := 0; i < k; i++ {
		var setName string
		for {
			fmt.Printf("Назва множини %d: ", i+1)
			setName = strings.TrimSpace(readLine())
			if setName == "" {
				fmt.Println("Порожня назва не допускається.")
				continue
			}
			if _, exists := sets[setName]; exists {
				fmt.Println("Ім'я вже використане — введіть інше.")
				continue
			}
			break
		}

		count := readInt(fmt.Sprintf("Скільки вершин буде в множині %q: ", setName))
		if count < 0 || count > len(vertices) {
			fmt.Println("Невірна кількість для множини — встановлюю 0.")
			count = 0
		}
		fmt.Printf("Введіть %d імен вершин для множини %q (через пробіл або по одному в рядку):\n", count, setName)
		setList := make([]string, 0, count)
		for len(setList) < count {
			remaining := count - len(setList)
			fmt.Printf("Залишилось ввести %d: ", remaining)
			parts := strings.Fields(readLine())
			for _, p := range parts {
				if !vertexSet[p] {
					fmt.Printf("Вершина %q не існує в графі — ігнорую.\n", p)
					continue
				}
				if vertexBelongs[p] {
					fmt.Printf("Вершина %q вже належить іншій множині — ігнорую (множини повинні бути неперетинні).\n", p)
					continue
				}
				setList = append(setList, p)
				vertexBelongs[p] = true
				if len(setList) == count {
					break
				}
			}
		}
		sets[setName] = setList
	}

	fmt.Println("\n--- Вхідні дані зчитано. Починаю обчислювати індекси залежності ---")

	weightGraph := make(map[string]map[string]int)
	setNames := make([]string, 0, len(sets))
	for name := range sets {
		setNames = append(setNames, name)
		weightGraph[name] = make(map[string]int)
	}

	for _, mName := range setNames {
		for _, nName := range setNames {
			if mName == nName {

			}
			count := 0
			for _, v := range sets[mName] {
				for _, u := range sets[nName] {
					if edges[v] != nil && edges[v][u] {
						count++
					}
				}
			}
			if count > 0 {
				weightGraph[mName][nName] = count
			}
		}
	}

	fmt.Println("\n=== Зважений орієнтований граф множин ===")
	if len(setNames) == 0 {
		fmt.Println("Немає множин — результат порожній граф.")
		return
	}
	for _, from := range setNames {
		adj := weightGraph[from]
		if len(adj) == 0 {
			fmt.Printf("%s: (немає вихідних ребер)\n", from)
			continue
		}
		parts := make([]string, 0, len(adj))
		for to, w := range adj {
			parts = append(parts, fmt.Sprintf("%s (вага=%d)", to, w))
		}
		fmt.Printf("%s -> %s\n", from, strings.Join(parts, ", "))
	}

	fmt.Println("\nМатриця ваг (рядки — from, стовпці — to):")

	fmt.Print("\t")
	for _, to := range setNames {
		fmt.Print(to + "\t")
	}
	fmt.Println()
	for _, from := range setNames {
		fmt.Print(from + "\t")
		for _, to := range setNames {
			w := weightGraph[from][to]
			fmt.Printf("%d\t", w)
		}
		fmt.Println()
	}

	fmt.Println("\nГотово.")
}
