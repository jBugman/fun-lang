package main

import "fmt"

type person struct {
	name string
	age  int
}

func main() {
	fmt.Println(person{name: "Alice", age: 30})
	fmt.Println(person{name: "Fred"})
	fmt.Println(&person{name: "Ann", age: 40})
	var s = person{name: "Sean", age: 50}
	fmt.Println(s.name)
	var sp = &s
	fmt.Println(sp.age)
	sp.age = 51
	fmt.Println(sp.age)
}
