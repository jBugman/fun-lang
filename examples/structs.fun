package main

struct person {
    name :: string
    age  :: int
}

func main = do
    print person{name: "Bob", age: 20}

    // Omitted fields will be zero-valued.
    print person{name: "Fred"}

    // Access struct fields with a dot.
    // s := person{name: "Sean", age: 50}
    var s = person{name: "Sean", age: 50}
    print s.name

    // You can also use dots with struct pointers - the pointers are automatically dereferenced.
    // sp := &s
    var sp = &s
    print sp.age

    // Structs are mutable.
    sp.age = 51
    print sp.age
