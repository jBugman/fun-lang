package main

// Here’s a function that takes two ints and returns their sum as an int.
func plus (a :: int, b :: int) -> int = a + b

// func plusPlus(a, b, c int) int {
//    return a + b + c
// }
func plusPlus (a :: int, b :: int, c :: int) -> int = a + b + c

// func vals (int, int) {
//     return 3, 7
// }
func vals () -> (int, int) = (3, 7)

// func intSeq() func() int
func intSeq () -> (lambda int) = inline {
    i := 0
    return func() int {
        i += 1
        return i
    }
}

func main = do
    var res = plus 1 2
    print ("1+2 =", res)
    res = plusPlus 1 2 3
    print ("1+2+3 =", res)

    var a, b = vals
    print a
    print b
    var _, c = vals
    print c
