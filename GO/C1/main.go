package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

type MemoryManager struct {
	MaxWidth    int
	Allocations map[int]int
	Size        int
}

func NewMemoryManager(size int, maxWidth int) *MemoryManager {
	return &MemoryManager{
		MaxWidth:    maxWidth,
		Allocations: make(map[int]int),
		Size:        size,
	}
}

func (m *MemoryManager) Allocate(size int) (int, bool) {
	index := 0

	keys := make([]int, 0, len(m.Allocations))
	for k := range m.Allocations {
		keys = append(keys, k)
	}
	sort.Ints(keys)

	for _, allocIndex := range keys {
		allocSize := m.Allocations[allocIndex]
		if allocIndex-index >= size {
			m.Allocations[index] = size
			return index, true
		}
		index = allocIndex + allocSize
	}
	if m.Size-index >= size {
		m.Allocations[index] = size
		return index, true
	}
	return -1, false
}

func (m *MemoryManager) Free(start int) bool {
	_, ok := m.Allocations[start]
	if !ok {
		return false
	}
	delete(m.Allocations, start)
	return true
}

func (m *MemoryManager) PrintMemory() {
	maxBlockSize := len(strconv.Itoa((m.Size - 1))) + 1
	lastAllocationStart := -1
	lastAllocationEnd := -1
	for i := 0; i < m.Size; i += m.MaxWidth {
		end := i + m.MaxWidth
		fmt.Print("|")
		for j := i; j < end; j += 1 {
			size, ok := m.Allocations[j]
			if ok {
				lastAllocationStart = j
				lastAllocationEnd = j + size
				lenOfNumber := len(strconv.Itoa(j))
				if j == i {
					fmt.Printf("%d%s", j, strings.Repeat("x", maxBlockSize-lenOfNumber))
				} else {
					fmt.Printf("|%d%s", j, strings.Repeat("x", maxBlockSize-lenOfNumber-1))
				}
			} else {
				if j == lastAllocationEnd && lastAllocationEnd != i {
					fmt.Printf("|%s", strings.Repeat(" ", maxBlockSize-1))
				} else if j > lastAllocationStart && j < lastAllocationEnd {
					fmt.Print(strings.Repeat("x", maxBlockSize))
				} else {
					fmt.Print(strings.Repeat(" ", maxBlockSize))
				}
			}
		}
		fmt.Println("|")
	}
}

func printHelp() {
	fmt.Println("Available commands:")
	fmt.Println("help - show this help")
	fmt.Println("exit - exit this program")
	fmt.Println("print - print memory blocks map")
	fmt.Println("allocate <num> - allocate <num> cells. Returns block first cell number")
	fmt.Println("free <num> - free block with first cell number <num>")
}

func main() {
	reader := bufio.NewReader(os.Stdin)
	fmt.Println("Please set memory size and max output width:")
	fmt.Print(">")

	line, _ := reader.ReadString('\n')
	line = strings.TrimSpace(line)
	parts := strings.Split(line, " ")
	if len(parts) < 2 {
		fmt.Println("Invalid input. Example: 30 10")
		return
	}
	size, _ := strconv.Atoi(parts[0])
	maxWidth, _ := strconv.Atoi(parts[1])

	manager := NewMemoryManager(size, maxWidth)
	fmt.Println("Type 'help' for additional info.")

	for {
		fmt.Print(">")
		line, _ := reader.ReadString('\n')
		line = strings.TrimSpace(line)
		parts := strings.Split(line, " ")

		switch parts[0] {
		case "help":
			printHelp()
		case "exit":
			return
		case "print":
			manager.PrintMemory()
		case "allocate":
			if len(parts) < 2 {
				fmt.Println("Usage: allocate <num>")
				continue
			}
			num, err := strconv.Atoi(parts[1])
			if err != nil {
				fmt.Println("Invalid number")
				continue
			}
			start, ok := manager.Allocate(num)
			if ok {
				fmt.Println(start)
			} else {
				fmt.Println("Not enough memory")
			}
		case "free":
			if len(parts) < 2 {
				fmt.Println("Usage: free <num>")
				continue
			}
			start, err := strconv.Atoi(parts[1])
			if err != nil {
				fmt.Println("Invalid number")
				continue
			}
			if !manager.Free(start) {
				fmt.Println("Invalid block id")
			}
		default:
			fmt.Println("Unknown command. Type 'help' for commands.")
		}
	}
}
