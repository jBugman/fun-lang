(package main

(func main (
  (var m (make (:map :string :int)))

  (set (val m "k1") 7)
  (set (val m "k2") 13)

  (print m)

  (var v1 (val m "k1"))
  (print "v1:" v1)

  (print "len:" (len m))

  (delete m "k2")
  (print "map:" m)

  (set _ prs (val m "k2"))
  (print "prs:" prs)

  (var n (lit (:map :string :int) (("foo" 1) ("bar" 2)) ))
  (print n)
)))
