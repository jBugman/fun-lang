(package main

(func intSeq (:func int) (
  (var i 0)
  (return (func () int) (
    (set i (+ i 1))
    (return i)
  ))
))

(func main (
  (var nextInt (intSeq))
  (print (nextInt))
  (print (nextInt))
  (print (nextInt))
  (var newInts (intSeq))
  (print (newInts))
)))
