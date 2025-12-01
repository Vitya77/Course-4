// main.go
package main

import (
	"bufio"
	"fmt"
	"hash/fnv"
	"strings"
	"sync"
)

type KeyValue struct {
	Key   string
	Value string
}

type MapFunc func(documentID string, contents string) []KeyValue

type ReduceFunc func(key string, values []string) string

type MapReduce struct {
	nReduce int
}

func NewMapReduce(nReduce int) *MapReduce {
	return &MapReduce{nReduce: nReduce}
}

func (mr *MapReduce) Run(inputs map[string]string, mapF MapFunc, reduceF ReduceFunc) map[string]string {
	intermediateCh := make([]chan KeyValue, mr.nReduce)
	for i := 0; i < mr.nReduce; i++ {
		intermediateCh[i] = make(chan KeyValue, 256)
	}

	var mapWg sync.WaitGroup
	for docID, contents := range inputs {
		mapWg.Add(1)
		go func(id, text string) {
			defer mapWg.Done()
			kvs := mapF(id, text)
			for _, kv := range kvs {
				idx := ihash(kv.Key) % mr.nReduce
				intermediateCh[idx] <- kv
			}
		}(docID, contents)
	}

	go func() {
		mapWg.Wait()
		for i := 0; i < mr.nReduce; i++ {
			close(intermediateCh[i])
		}
	}()

	results := make(map[string]string)
	var resMu sync.Mutex
	var reduceWg sync.WaitGroup
	for i := 0; i < mr.nReduce; i++ {
		reduceWg.Add(1)
		go func(ch chan KeyValue) {
			defer reduceWg.Done()

			local := make(map[string][]string)
			for kv := range ch {
				local[kv.Key] = append(local[kv.Key], kv.Value)
			}

			for k, vals := range local {
				out := reduceF(k, vals)
				resMu.Lock()
				results[k] = out
				resMu.Unlock()
			}
		}(intermediateCh[i])
	}

	reduceWg.Wait()
	return results
}

func ihash(s string) int {
	h := fnv.New32a()
	h.Write([]byte(s))
	return int(h.Sum32() & 0x7fffffff)
}

func mapWordCount(documentID string, contents string) []KeyValue {
	var result []KeyValue

	scanner := bufio.NewScanner(strings.NewReader(contents))
	scanner.Split(bufio.ScanWords)
	for scanner.Scan() {
		word := scanner.Text()

		word = strings.ToLower(word)
		word = strings.Trim(word, `.,!?;:"'()[]{}<>`)
		if word == "" {
			continue
		}
		result = append(result, KeyValue{Key: word, Value: "1"})
	}
	return result
}

func reduceWordCount(key string, values []string) string {
	count := 0
	for _, v := range values {
		count++
		_ = v
	}
	return fmt.Sprintf("%d", count)
}

func main() {
	docs := map[string]string{
		"doc1": "Hello world hello",
		"doc2": "World of Go. Hello, Go!",
		"doc3": "go go go, HELLO!",
	}

	mr := NewMapReduce(4)

	out := mr.Run(docs, mapWordCount, reduceWordCount)

	keys := make([]string, 0, len(out))
	for k := range out {
		keys = append(keys, k)
	}

	fmt.Println("Word counts:")
	for _, k := range keys {
		fmt.Printf("%s -> %s\n", k, out[k])
	}
}
