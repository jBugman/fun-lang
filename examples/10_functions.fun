(package main

(import "fmt")

(func plus ((a :int) (b :int)) :int
  (+ a b)
)

(func plusPlus ((a :int) (b :int) (c :int)) (
  (print (+ a b c))
))

(func main () (
  (var res (plus 1 2))
  (print "1+2 =" res)
  (fmt.Print "1+2+3 =")
  (plusPlus 1 2 3)
))
)