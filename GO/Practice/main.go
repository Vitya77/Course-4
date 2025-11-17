package main

import "fmt"

type I interface {
	M()
}
type T struct {
	S string
}

func (t *T) M() {
	if t == nil {
		fmt.Println("<nil>")
		return
	}
	fmt.Println(t.S)
}

func main() {
	var i I  // == nil
	var t *T // == nil
	i = t    // (nil, *T) != nil
	i.M()    // nil pointer ERROR ?
}
