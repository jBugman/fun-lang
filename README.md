## Fun
### Lisp-style language with static typing, compiled to Go

Idea is to provide alternative syntax with more syntax sugar using code-generation to allow more concise code.
Also, there may be an ability to use raw inline Go code for more complex cases.

⚠️ Under active development. Expect changes to everything.


    (package main

    (import "io")

    (func inc [(val :int)] [:int] (
      (+ val 1)))

    (func print42 (print 42))

    (func main (
      (var line "Hello World!")
      (fmt.Fprintln io.Discard line))))
