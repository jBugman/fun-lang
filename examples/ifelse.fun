package main

func main = do
    // if 7%2 == 0 {
    // 	fmt.Println("7 is even")
    // } else {
    // 	fmt.Println("7 is odd")
    // }
    if (7 % 2 == 0)
        then (print "7 is even")
        else (print "7 is odd")

    // if 8%4 == 0 {
    // 	fmt.Println("8 is divisible by 4")
    // }
    if (8 % 4 == 0) then (print "8 is divisible by 4")

    // if num := 9; num < 0 {
    // 	fmt.Println(num, "is negative")
    // } else if num < 10 {
    //  fmt.Println(num, "has 1 digit")
    // } else {
    //  fmt.Println(num, "has multiple digits")
    // }
    var num = 9
    switch num
        case num < 0  -> print (num, "is negative")
        case num < 10 -> print (num, "has 1 digit")
        default       -> print (num, "has multiple digits")
