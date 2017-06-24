package main

func main = do
    // Here we use range to sum the numbers in a slice. Arrays work like this too.
    var nums = [2, 3, 4]
    var sum = 0
    // for _, num := range nums {
    //     sum += num
    // }
    for (_, j = range nums)
        sum += num
    print ("sum:", sum)

    // for i, num := range nums {
    //    if num == 3 {
    //        fmt.Println("index:", i)
    //    }
    // }
    for (i, num = range nums)
        if (num == 3)
            print ("index:", i)

    // range on map iterates over key/value pairs.
    // kvs := map[string]string{"a": "apple", "b": "banana"}
    // for k, v := range kvs {
    //    fmt.Printf("%s -> %s\n", k, v)
    // }
    var kvs = {"a": "apple", "b": "banana"}
    for (k, v = range kvs)
        printf "%s -> %s\n" (k, v)

    // range can also iterate over just the keys of a map.
    for (k = range kvs)
        print ("key:", k)

    // range on strings iterates over Unicode code points.
    // The first value is the starting byte index of the rune and the second the rune itself.
    for (i, c = range "go")
        println (i, c)
