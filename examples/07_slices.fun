(package main

(func main (
  (var s (make (:slice :string) 3))
  (print "emp:" s)

  (set (val s 0) "a")
  (set (val s 1) "b")
  (set (val s 2) "c")
  (print "set:" s)
  (print "get:" (val s 2))

  (print "len:" (len s))

  (set s (append s "d"))

  (var c (make (:slice :string) (len s)))
  (copy c s)
  (print "cpy:" c)

  (var l (slice s 2 5))

  (set l (slice s _ 5))

  (set l (slice s 2 _))

  (var t (:slice :string) ("g" "h" "i"))
  (print "dcl:" t)

  (var twoD (make (:slice (:slice :int)) 3))
  (for i 0 3 (
    (var innerLen (+ i 1))
    (set (val twoD i) (make (:slice :int) innerLen))
    (for j 0 innerLen
      (set (val twoD i j) (+ i j))  )))  ; two dimensional-slice access could be better
)) )
