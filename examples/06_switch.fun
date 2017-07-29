(package main

(import "fmt")
(import "time")

(func main (
  (var i 2)
  (fmt.Print "Write " i " as ")
  (switch i (
    (case 1 (fmt.Println "one"))
    (case 2 (print "two"))
    (case 3 (print "three"))
  ))

  (switch (. time (Now) (Weekday)) (
    (case time.Saturday time.Sunday
      (print "It's the weekend"))
    (default (print "It's a weekday"))
  ))

  (var t (time.Now))
  (switch (
    (case (< (t.Hour) 12) (print "It's before noon"))
    (default (print "It's after noon"))
  ))

  ; anonimous function
  (var whatAmI (func ((i :any))
    (switch (type t i) (
      (case :bool (print "I'm a bool"))
      (case :int (print "I'm an int"))
      (default (printf "Don't know type %T\n" t))
    ))
  ) )
  
  (whatAmI true)
  (whatAmI 1)
  (whatAmI "hey")
))
)
