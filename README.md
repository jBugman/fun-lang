# Fun

[![CircleCI](https://img.shields.io/circleci/project/github/jBugman/fun-lang/master.svg?label=master)](https://circleci.com/gh/jBugman/fun-lang)
[![CircleCI](https://img.shields.io/circleci/project/github/jBugman/fun-lang/dev.svg?label=dev)](https://circleci.com/gh/jBugman/fun-lang)

Lisp-style language with static typing, compiled to Go.

Idea is to provide alternative syntax with more syntax sugar using code-generation to allow more concise code.
Also, there may be an ability to use raw inline Go code for more complex cases.

⚠️ Under active development. Expect changes to everything.

⚠️ Examples in `examples/` may not be up to date.

Transpiling is mostly literal, but there are some differences from Go:

* No arrays, only slices
* No `:=`, only `var` everywhere
* No named return values
* No `new`
* No `goto`
* No raw string literals (yet?)
* No declaring in if statement (yet?)
* Can't declare but can call a function with variadic parameters (yet?)


### Learn Fun in Y minutes (translated from Go)

```lisp
; Single line comment

(package main

; Import declaration declares library packages referenced in this file.
(import "fmt")        ; A package in the Go standard library.
(import "io/ioutil")  ; Implements some I/O utility functions.
(import "math" "m")   ; Math library with local alias m.
(import "net/http")   ; Yes, a web server!
(import "os")         ; OS functions like working with the file system
(import "strconv")    ; String conversions.

; A function definition.
(func main () (
  ; Println outputs a line to stdout.
  ; Qualify it with the package name, fmt.
  (fmt.Println "Hello world!")

  ; Call another function within this package.
  (beyondHello)  ; A single identifier in parens is a function call without arguments.
))

; Functions have parameters in parentheses.
; If there are no parameters, empty parentheses are still required.
(func beyondHello () (
  (var x :int)  ; Variable declaration. Variables must be declared before use.
                ; Type literals begin with a colon.

  (set x 3)     ; Variable assignment.

  (var sum prod (learnMultiple x y))  ; Function returns two values.
  (fmt.Println "sum:" sum)            ; Simple output.
  (print "prod:" prod)                ; print is a synonym for fmt.Println
  (learnTypes)
)
  
; Functions can have parameters and (multiple!) return values.
; Here `x`, `y` are the arguments and two ints is the signature (what's returned).
(func learnMultiple ((x :int) (y :int)) (:int :int) (
  (return (+ x y) (* x y))  ; Return two values.
))

; Some built-in types and literals.
(func learnTypes () (
  ; Variable declaration usually gives you what you want.
  (var str "Learn Fun!")  ; string type.

  ; TODO: not supported yet ("raw" string literal)
  ; s2 := `A "raw" string literal
  ; can include line breaks.` // Same string type.

  ; Non-ASCII literal. Fun source is UTF-8.
  (var g 'Σ')  ; char type, an alias for int32, holds a unicode code point.

  (var f 3.14195)         ; float64, an IEEE-754 64-bit floating point number.
  (var c (:complex 3 4))  ; complex128, represented internally with two float64's.

  ; var syntax with initializers.
  (var u :uint 7)               ; Unsigned, but implementation dependent size as with int.
  (var pi :float32 (/ 22.0 7))

  ; Conversion syntax. byte is an alias for uint8.
  (var n (:byte '\n'))  ; TODO: subject to change (conversion syntax)

  ; Arrays have size fixed at compile time. But Fun do not support them (yet?).
  ; var a4 [4]int  // An array of 4 ints, initialized to all 0.

  ; Slices have dynamic size. Arrays and slices each have advantages
  ; but use cases for slices are much more common.
  (var s3 (:slice :int) (4 5 9))
  (var s4 (make (:slice :int) 4))      ; Allocates slice of 4 ints, initialized to all 0.
  (var d2 (:slice (:slice :float64)))  ; Declaration only, nothing allocated here.
  (var bs ((:slice :byte) "a slice"))  ; Type conversion syntax.

  ; Because they are dynamic, slices can be appended to on-demand.
  ; To append elements to a slice, the built-in append() function is used.
  ; First argument is a slice to which we are appending. Commonly,
  ; the array variable is updated in place, as in example below.
  (var s (:slice :int) (1 2 3))  ; Result is a slice of length 3.
  (set s (append s 4 5 6))       ; Added 3 elements. Slice now has length of 6.
  (print s)                      ; Updated slice is now [1 2 3 4 5 6]

  (var s1 (:slice :int) (7 8 9))
  (set s (concat s s1))  ; Like append, but second argument is another slice.
  (print s)              ; Updated slice is now [1 2 3 4 5 6 7 8 9]

  (var p q (learnMemory))  ; Declares p, q to be type pointer to int.
  (print *p *q)            ; * designates a pointer. This prints two ints.
                           ; TODO: subject to change (pointer dereference)

  ; Maps are a dynamically growable associative array type, like the
  ; hash or dictionary types of some other languages.
  (set m (:map :string :int) (("three" 3) ("four" 4)))
  (set (val m "one") 1)  ; val == map[key]

  ; Unused variables are an error in Go.
  ; The underscore lets you "use" a variable but discard its value.
  (var file _ (os.Create "output.txt"))
  (fmt.Fprint file "This is how you write to a file, by the way")
  (file.Close)

  ; Output of course counts as using a variable.
  (print s c a4 s3 d2 m)

  (learnFlowControl)
))

; In Go it is possible, unlike in many other languages for functions
; to have named return values.
; It is not supported in Fun (yet?). 
; // func learnNamedReturns(x, y int) (z int)

; Go is fully garbage collected. It has pointers but no pointer arithmetic.
; You can make a mistake with a nil pointer, but not by incrementing a pointer.
(func learnMemory () ((:ptr :int) (:prt :int)) (
  (var p int)                      ; int is initialized to 0.
  (var s (make (:slice :int) 20))  ; Allocate 20 ints as a single block of memory.
  (set (val s 3) 7)                ; Assign one of them.
  (var r -2)                       ; Declare another local variable.
  (return (& (val s 3)) (& r))     ; & takes the address of an object.
))

; Implicit return of last expression results if there is return list declaration.
(func expensiveComputation () :float64 (m.Exp 10))

(func learnFlowControl () (
  ; If statements require brace brackets, and do not require parentheses.
  (if true (print "told ya"))

  ; Use switch in preference to chained if statements.
  (var x 42.0)
  (switch x
    (case 0)
    (case 1)
    (case 42 (
      ; Cases don't "fall through". There is a `fallthrough` keyword however.
    ))
    (case 43)  ; Unreached.
    (default)  ; Default case is optional.
  )

  ; Variables declared in for and if are local to their scope.
  (for x 0 3
    (print "iteration" x)
  )
  ; x == 42 here.

  ; For is the only loop statement in Go, but it has alternate forms.
  (for (        ; Infinite loop.
    (break)     ; Just kidding.
    (continue)  ; Unreached.
  ))

  ; You can use range to iterate over a slice, a string, a map, or a channel.
  ; range returns one (channel) or two values (array, slice, string and map).
  (var mp (:map :string :int) (("one" 1) ("two" 2) ("three" 3)))
  (for (range key value mp) (
    ; for each pair in the map, print key and value
    (fmt.Printf "key=%s, value=%d\n" key value)
  ))
  ; If you only need the value, use the underscore as the key
  (for (range _ name ((:slice :string) ("Bob" "Bill" "Joe")) )
    (fmt.Printf "Hello, %s\n" name)
  )

  ; You can declare and assign variable in an if statement, then test.
  ; But it not supported yet.
  ; // if y := expensiveComputation(); y > x { x = y }

  ; Function literals are closures.
  (var xBig (func () :bool (
    (> x 10000)  ; References x declared above switch statement.
  )))
  (set x 99999)

  (print "xBig:" (xBig))  ; true
  (set x 1.3e3)           ; This makes x == 1300
                          ; But e-notation is not supported (yet)
  (print "xBig:" (xBig))  ; false now.

  ; What's more is function literals may be defined and called inline,
  ; acting as an argument to function, as long as:
  ; a) function literal is called immediately (),
  ; b) result type matches expected type of argument.
  (print "Add + double two numbers: "
    ((func ((a :int) (b :int)) :int (
      (* (+ a b) 2)
    )) 10 2)  ; Called with args 10 and 2
  )
  ; => Add + double two numbers: 24
  ; But it's not very readable and may not be supported.

  ; goto is not supported
  ; // goto love
  ; // love:

  ; (learnFunctionFactory)  ; func returning func
  (learnDefer)            ; A quick detour to an important keyword.
  (learnInterfaces)       ; Good stuff coming up!
))

; Function literals are hard to read and this pattern is even worse.
; So maybe it will work one day, or maybe not.
;
; (func learnFunctionFactory () (
;   ; Next two are equivalent, with second being more practical
;   (print ((sentenceFactory "summer") "A beautiful" "day!"))
;
;   (var d (sentenceFactory "summer"))
;   (print (d "A beautiful" "day!"))
;   (print (d "A lazy" "afternoon!"))
; ))
;
; Decorators are common in other languages. Same can be done in Fun
; with function literals that accept arguments.
; // func sentenceFactory(mystring :string) func(before, after string) string {
; //   return func(before, after string) string {
; //       return fmt.Sprintf("%s %s %s", before, mystring, after) // new string
; //   }
; // }

(func learnDefer () :bool (
  ; Deferred statements are executed just before the function returns.
  (defer (fmt.Println "deferred statements execute in reverse (LIFO) order."))
  (defer (print "\nThis line is being printed first because"))

  ; Defer is commonly used to close a file, so the function closing the
  ; file stays close to the function opening the file.
  (return true)
))

; Define Stringer as an interface type with one method, String.
(interface Stringer (
  (String () :string)
))

; Define pair as a struct with two fields, ints named x and y.
(struct pair (
  (x :int)
  (y :int)
))

; Define a method on type pair. Pair now implements Stringer.
(method (p :pair) String () :string (  ; p is called the "receiver"
  ; Sprintf is another public function in package fmt.
  ; Dot syntax references fields of p.
  (return (fmt.Sprintf "(%d, %d)" p.x p.y))
))

(func learnInterfaces () (
  ; Struct literal evaluates to an initialized struct.
  (var p (:pair (3 4))

  (print (p.String))  ; Call String method of p, of type pair.
  (var i :Stringer)   ; Declare i of interface type Stringer.
  (set i p)           ; Valid because pair implements Stringer
  ; Call String method of i, of type Stringer. Output same as above.
  (fmt.Println (i.String))

  ; Functions in the fmt package call the String method to ask an object
  ; for a printable representation of itself.
  (fmt.Println p)  ; Output same as above. Println calls String method.
  (fmt.Println i)  ; Output same as above.

  (learnErrorHandling)
))

; Go functions can have variadic parameters, and you can call them.
; But you can not declare them in Fun (yet).
; func LearnVariadicParams(myStrings ...interface{}) {
;    // Iterate each value of the variadic.
;    // The underbar here is ignoring the index argument of the array.
;    for _, param := range myStrings {
;        fmt.Println("param:", param)
;    }
;
;    // Pass variadic value as a variadic parameter.
;    fmt.Println("params:", fmt.Sprintln(myStrings...))
;
;    learnErrorHandling()
; }

(func learnErrorHandling () (
  ; ", ok" idiom used to tell if something worked or not.
  ; There is no direct alternative in Fun (yet).
  (var m (:map :int :string) ((3 "three") (4 "four")))

  (var x ok (val m 1))  ; ok will be false because 1 is not in the map.
  (if ok
    (print "no one there")
    (fmt.Print x)  ; x would be the value, if it were in the map.
  )

  ; An error value communicates not just "ok" but more about the problem.
  (var _ err (strconv.Atoi "non-int"))
  (if (!= err nil) (
    ; prints 'strconv.ParseInt: parsing "non-int": invalid syntax'
    (print err)
  ))

  ; We'll revisit interfaces a little later. Meanwhile,
  (learnConcurrency)
))

; c is a channel, a concurrency-safe communication object.
(func inc ((i :int) (c (:chan :int))) (
  (<- c (+ i 1))  ; <- is the "send" operator when a channel appears on the left.
))

; We'll use inc to increment some numbers concurrently.
(func learnConcurrency () (
  ; Same make function used earlier to make a slice. Make allocates and
  ; initializes slices, maps, and channels.
  (var c (make (:chan :int)))

  ; Start three concurrent goroutines. Numbers will be incremented
  ; concurrently, perhaps in parallel if the machine is capable and
  ; properly configured. All three send to the same channel.
  (go (inc 0 c))  ; go is a statement that starts a new goroutine.
  (go (inc 10 c))
  (go (inc -805 c))
  
  ; Read three results from the channel and print them out.
  ; There is no telling in what order the results will arrive!
  (print (<- c) (<- c) (<-c))  ; channel on right, <- is "receive" operator.

  (var cs (make (:chan :string)))           ; Another channel, this one handles strings.
  (var ccs (make (:chan (:chan :string))))  ; A channel of string channels.
  (go (func () (<- c 84)))                  ; Start a new goroutine just to send a value.
  (go (func () (<- cs "wordy")))            ; Again, for cs this time.

  ; Select has syntax like a switch statement but each case involves
  ; a channel operation. It selects a case at random out of the cases
  ; that are ready to communicate.
  (select
    (case (var i (<- c))  ; The value received can be assigned to a variable,
      (fmt.Printf "it's a %T" i)
    )
    (case (<- cs)         ; or the value received can be discarded.
      (print "it's a string")
    )
    (case (<- ccs)        ; Empty channel, not ready for communication.
      (print "didn't happen.")
    )
  )
    ; At this point a value was taken from either c or cs. One of the two
    ; goroutines started above has completed, the other will remain blocked.

  (learnWebProgramming)  ; Go does it. You want to do it too.
))

; A single function from package http starts a web server.
(func learnWebProgramming () (
  ; First parameter of ListenAndServe is TCP address to listen to.
  ; Second parameter is an interface, specifically http.Handler.
  (go (func (
    (var err (http.ListenAndServe ":8080" (pair)))
    (print err)  ; don't ignore errors
  )))
  (requestServer)
))

; Make pair an http.Handler by implementing its only method, ServeHTTP.
(method (:pair) ServeHTTP ((w :http.ResponseWriter) (r (:ptr :http.Request))) (
  ; Serve data with a method of http.ResponseWriter.
  (w.Write ((:slice :byte) "You learned Go in Y minutes!"))
))

(func requestServer () (
  (var resp err (http.Get "http://localhost:8080"))
  (print err)
  (defer (resp.Body.Close))
  (var body err (ioutil.ReadAll resp.Body))
  (fmt.Printf "\nWebserver said: `%s`" (:string body))
))

)
```
