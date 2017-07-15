package main

import "fmt"

func main() {
	var i = 1
	for i < 3 {
		fmt.Println(i)
		i = i + 1
	}
	for i := 7; i < 9; i++ {
		fmt.Println(i)
	}
	for {
		fmt.Println("loop")
		break
	}
	for n := 0; n < 5; n++ {
		if n%2 == 0 {
			continue
		}
		fmt.Println(n)
	}
}
