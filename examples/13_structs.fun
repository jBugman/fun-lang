(package main

(struct person
  (name :string)
  (age  :int)
)

(func main (
  (print (:person ((name "Alice") (age 30))))
  (print (:person ((name "Fred"))))
  (print (& (:person ((name "Ann") (age 40)))))
  (var s (:person ((name "Sean") (age 50))))
  (print s.name)
  (var sp (& s))
  (print sp.age)
  (set sp.age 51)
  (print sp.age) 
))
)
