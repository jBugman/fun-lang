package main

struct rect {
    width  :: int
    height :: int
}

// This area method has a receiver type of *rect.
// func (r *rect) area() int {
//     return r.width * r.height
// }
method (r :: @rect) area () -> int = r.width * r.height


// Methods can be defined for either pointer or value receiver types. Here’s an example of a value receiver.
// func (r rect) perim() int {
//     return 2*r.width + 2*r.height
// }
method (r :: rect) perim () -> int = 2 * r.width + 2 * r.height

func main = do
    var r = rect{width: 10, height: 5}

    print ("area: ", r.area)
    print ("perim:", r.perim)

    var rp = &r
    print ("area: ", rp.area)
    print ("perim:", rp.perim)
