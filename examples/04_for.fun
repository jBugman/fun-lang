(package main

(func main (
  (var i 1)
  (for (< i 3) (
    (print i)
    (set i (+ i 1)) ))

  (for (i 7 9) (print i))

  (for (
    (print "loop")
    (break) ))

  (for (n 0 5) (
    (if (= (% n 2) 0) (continue))
    (print n) ))
)))
