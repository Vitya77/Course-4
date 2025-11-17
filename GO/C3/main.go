package main

import (
	"bufio"
	"fmt"
	"hash/fnv"
	"os"
)

type HashSet struct {
	buckets [][]string
	size    int
}

func NewHashSet(size int) *HashSet {
	return &HashSet{
		buckets: make([][]string, size),
		size:    size,
	}
}

func hash(s string) uint32 {
	h := fnv.New32a()
	h.Write([]byte(s))
	return h.Sum32()
}

func (hs *HashSet) Add(s string) {
	idx := int(hash(s)) % hs.size

	for _, v := range hs.buckets[idx] {
		if v == s {
			return
		}
	}
	hs.buckets[idx] = append(hs.buckets[idx], s)
}

func (hs *HashSet) Contains(s string) bool {
	idx := int(hash(s)) % hs.size
	for _, v := range hs.buckets[idx] {
		if v == s {
			return true
		}
	}
	return false
}

func (hs *HashSet) Elements() []string {
	var res []string
	for _, bucket := range hs.buckets {
		res = append(res, bucket...)
	}
	return res
}

func readFileToSet(filename string, hs *HashSet) error {
	file, err := os.Open(filename)
	if err != nil {
		return err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		hs.Add(scanner.Text())
	}
	return scanner.Err()
}

func setsEqual(a, b *HashSet) bool {
	aElems := a.Elements()
	bElems := b.Elements()

	for _, v := range aElems {
		if !b.Contains(v) {
			return false
		}
	}

	for _, v := range bElems {
		if !a.Contains(v) {
			return false
		}
	}
	return true
}

func main() {
	if len(os.Args) < 3 {
		fmt.Println("Використання: go run main.go file1.txt file2.txt")
		return
	}

	file1 := os.Args[1]
	file2 := os.Args[2]

	set1 := NewHashSet(100003)
	set2 := NewHashSet(100003)

	if err := readFileToSet(file1, set1); err != nil {
		fmt.Println("Помилка читання файлу 1:", err)
		return
	}
	if err := readFileToSet(file2, set2); err != nil {
		fmt.Println("Помилка читання файлу 2:", err)
		return
	}

	if setsEqual(set1, set2) {
		fmt.Println("Множини співпадають ✅")
	} else {
		fmt.Println("Множини різні ❌")
	}
}
