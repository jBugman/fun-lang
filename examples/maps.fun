package main


func main = do
    // m := make(map[string]int)
    var m = makeMap string int

    // Set key/value pairs using typical name[key] = val syntax.
    m["k1"] = 7
    m["k2"] = 13

    // Printing a map with e.g. Println will show all of its key/value pairs.
    print m

    // Get a value for a key with name[key].
    var v1 = m["k1"]
    print ("v1: ", v1)

    // The builtin len returns the number of key/value pairs when called on a map.
    print ("len:", len m)

    // The builtin delete removes key/value pairs from a map.
    delete m "k2"
    print ("map:", m)

    // The optional second return value when getting a value from a map indicates if the key was present in the map.
    // This can be used to disambiguate between missing keys and keys with zero values like 0 or "".
    // _, prs := m["k2"]
    // var prs = hasKey m "k2"
    print ("prs:", prs)

    // You can also declare and initialize a new map in the same line with this syntax.
    // n := map[string]int{"foo": 1, "bar": 2}
    var n = {"foo": 1, "bar": 2}
    print n
