package main

func main = do
    // i := 1
    // for i <= 3 {
    // 	fmt.Println(i)
    // 	i = i + 1
    // }
    var i = 1
    for (i < 3) do
        print i
        i = i + 1

    // for j := 7; j <= 9; j++ {
    // 	fmt.Println(j)
    // }
    for (i = range' 7 9) print i

    // for {
    // 	fmt.Println("loop")
    // 	break
    // }
    for do
        print "loop"
        break

    // for n := 0; n <= 5; n++ {
    // 	if n%2 == 0 {
    // 		continue
    // }
    // 	fmt.Println(n)
    // }
    for (n = range' 0 5) do
        if (n % 2 == 0) then continue
        print n
