(package main

(func main (
  (var s (make :[]string 3))
  (print "emp:" s)

  (set (nth s 0) "a")
  (set (nth s 1) "b")
  (set (nth s 2) "c")
  (print "set:" s)
  (print "get:" (nth s 2))

  (print "len:" (len s))

  (set s (append s "d"))

  (var c (make :[]string (len s)))
  (copy c s)
  (print "cpy:" c)

  (var l (slice s 2 5))

  (set l (slice s _ 5))

  (set l (slice s 2 _))

  (var t ["g" "h" "i"])
  (print "dcl:" t)

  (var twoD (make :[][]int 3))
  (for (i 0 3) (
    (var innerLen (+ i 1))
    (set (nth twoD i) (make :[]int innerLen))
    (for (j 0 innerLen)
      (set (nth (nth twoD i) j) (+ i j))  )))))) ; two dimensional-slice access could be better
