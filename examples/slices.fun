package main

func main = do

    // s := make([]string, 3)
    // fmt.Println("emp:", s)
    var s = makeSlice string 3
    print ("emp:", s)

    // We can set and get just like with arrays.
    s[0] = "a"
    s[1] = "b"
    s[2] = "c"
    print ("set:", s)
    print ("get:", s[2])

    // `len` returns the length of the slice as expected.
    print ("len:", len s)

    // s = append(s, "d")
    s = s ++ "d"

    // c := make([]string, len(s))
    // copy(c, s)
    // fmt.Println("cpy:", c)
    var c = makeSlice string (len s)
    copy c s
    print ("cpy:", c)

    // Slices support a "slice" operator with the syntax `slice[low:high]`.
    // l := s[2:5]
    var l = s[2:5]

    // This slices up to (but excluding) `s[5]`.
    l = s[:5]

    // And this slices up from (and including) `s[2]`.
    l = s[2:]

    // We can declare and initialize a variable for slice in a single line as well.
    // t := []string{"g", "h", "i"}
    var t = ["g", "h", "i"]
    print ("dcl:", t)

    // twoD := make([][]int, 3)
    // for i := 0; i < 3; i++ {
    //     innerLen := i + 1
    //     twoD[i] = make([]int, innerLen)
    //     for j := 0; j < innerLen; j++ {
    //         twoD[i][j] = i + j
    //   }
    var twoD = makeSlice [string] 3
    for (i = range 0 3) do
        var innerLen = i + 1
        twoD[i] = makeSlice int innerLen
        for (j = range 0 innerLen)
            twoD[i][j] = i + j
