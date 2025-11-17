package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"
)

type Route struct {
	StartStation string
	EndStation   string
	Stops        int
	Length       float64
}

func main() {
	file, err := os.Open("routes.txt")
	if err != nil {
		fmt.Println("Помилка відкриття файлу:", err)
		return
	}
	defer file.Close()

	var routes []Route
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		var r Route
		line := scanner.Text()
		parts := strings.Fields(line)
		if len(parts) != 4 {
			fmt.Println("Некоректний рядок:", line)
			continue
		}
		fmt.Sscanf(line, "%s %s %d %f", &r.StartStation, &r.EndStation, &r.Stops, &r.Length)
		routes = append(routes, r)
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Помилка читання файлу:", err)
		return
	}

	sort.Slice(routes, func(i, j int) bool {
		return routes[i].Length < routes[j].Length
	})

	fmt.Println("\nВсі маршрути (відсортовані за протяжністю):")
	for _, r := range routes {
		fmt.Printf("%s → %s | Зупинок: %d | Довжина: %.1f км\n", r.StartStation, r.EndStation, r.Stops, r.Length)
	}

	var X float64
	fmt.Print("\nВведіть X (середня довжина між зупинками): ")
	fmt.Scan(&X)

	count := 0
	for _, r := range routes {
		if float64(r.Length)/float64(r.Stops) < X {
			count++
		}
	}

	fmt.Printf("\nКількість маршрутів, де середня довжина між зупинками < %.1f км: %d\n", X, count)

	var station string
	fmt.Print("\nВведіть початкову станцію X: ")
	fmt.Scan(&station)

	var startRoutes []Route
	for _, r := range routes {
		if strings.EqualFold(r.StartStation, station) {
			startRoutes = append(startRoutes, r)
		}
	}

	if len(startRoutes) == 0 {
		fmt.Printf("Немає маршрутів, що починаються в станції '%s'\n", station)
	} else {
		fmt.Printf("\nМаршрути, що починаються в '%s':\n", station)
		for _, r := range startRoutes {
			fmt.Printf("%s → %s | Зупинок: %d | %.1f км\n", r.StartStation, r.EndStation, r.Stops, r.Length)
		}
	}

	maxStops := 0
	for _, r := range routes {
		if r.Stops > maxStops {
			maxStops = r.Stops
		}
	}

	fmt.Printf("\nМаршрути з максимальною кількістю зупинок (%d):\n", maxStops)
	for _, r := range routes {
		if r.Stops == maxStops {
			fmt.Printf("%s → %s | %.1f км\n", r.StartStation, r.EndStation, r.Length)
		}
	}
}
