(package main

(func zeroval ((ival :int))
  (set ival 0) )

(func zeroptr ((iptr (:ptr :int)))
  (set (* iptr) 0) )

(func main (
  (var i 1)
  (print "initial:" i)
  (zeroval i)
  (print "zeroval:" i)
  (zeroptr (& i))
  (print "zeroptr:" i)
  (print "pointer:" (& i))
)) )
