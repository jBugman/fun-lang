(package main
(import "fmt")
(import "time")

(func main (
  (var i 2)
  (fmt.Print "Write " i " as ")
  (switch i
    (case 1 (fmt.Println "one"))
    (case 2 (fmt.Println "two"))
    (case 3 (print "three"))
  )

  (switch (.Weekday (time.Now ()) ())  ; TODO: Issue #33
    (case (time.Saturday time.Sunday)
      (fmt.Println "It's the weekend"))
    (default (fmt.Println "It's a weekday"))
  )

  (var t (time.Now ()))
  (switch ()
    (case (< (t.Hour ()) 12) (fmt.Println "It's before noon"))
    (default (fmt.Println "It's after noon"))
  )

  ; anonimous function
  (var whatAmI (func (i :any) (
    (switch (type t i)
      (case :bool (fmt.Println "I'm a bool"))
      (case :int (fmt.Println "I'm an int"))
      (default (fmt.Printf "Don't know type %T\n" t))
    )
  )))
  
  (whatAmI true)
  (whatAmI 1)
  (whatAmI "hey")
)))