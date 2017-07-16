package main

import "fmt"

func main() {
	var s = make([]string, 3)
	fmt.Println("emp:", s)
	s[0] = "a"
	s[1] = "b"
	s[2] = "c"
	fmt.Println("set:", s)
	fmt.Println("get:", s[2])
	fmt.Println("len:", len(s))
	s = append(s, "d")
	var c = make([]string, len(s))
	copy(c, s)
	fmt.Println("cpy:", c)
	var l = s[2:5]
	l = s[:5]
	l = s[2:]
	var t = []string{"g", "h", "i"}
	fmt.Println("dcl:", t)
	var twoD = make([][]int, 3)
	for i := 0; i < 3; i++ {
		var innerLen = i + 1
		twoD[i] = make([]int, innerLen)
		for j := 0; j < innerLen; j++ {
			twoD[i][j] = i + j
		}
	}
}
