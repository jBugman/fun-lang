(package main

(func main (
  (if (== (% 7 2) 0)
    (print "7 is even")
    (print "7 is odd"))

  (if (== (% 8 4) 0) (print "8 is divisible by 4"))

  (var num 9)
  (if (< num 0)
    (print num "is negative")
    (if (< num 10)
      (print num "has 1 digit")
      (print num "has multiple digits"))))))
