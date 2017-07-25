(package main

(func main (
  (var nums ((:slice :int) (2 3 4)))
  (var sum 0)
  (for (range _ num nums) (
      (set sum (+ sum num)) ))
  (print "sum:" sum)

  (for (range i num nums) (
    (if (= num 3)
      (print "index:" i) ) ))

  (var kvs ((:map :string :string) (("a" "apple") ("b" "banana"))) )
  (for (range k v kvs)
    (printf "%s -> %s\n" k v) )

  (for (range k kvs)
    (print "key:" k) )

  (for (range i c "go")
    (print i c) )
)))