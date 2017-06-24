package main

// func zeroval(ival int) {
//    ival = 0
// }
func zeroval (ival :: int) = ival = 0

// zeroptr in contrast has an *int parameter, meaning that it takes an int pointer.
// The *iptr code in the function body then dereferences the pointer from its memory
// address to the current value at that address.
// Assigning a value to a dereferenced pointer changes the value at the referenced address.
// func zeroptr(iptr *int) {
//     *iptr = 0
// }
func zeroptr (iptr :: @int) = @iptr = 0

func main = do
    var i = 1
    print ("initial:", i)
    zeroval i
    print ("zeroval:", i)

    zeroptr &i
    print ("zeroptr:", i)
    print ("pointer:", &i)
}