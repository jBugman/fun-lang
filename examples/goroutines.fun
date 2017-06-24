package main

func f (from :: string) =  for (i = range 0 3) print (from, ":", i)

func main = do
    // Suppose we have a function call f(s). Here’s how we’d call that in the usual way, running it synchronously.
    f "direct"

    // To invoke this function in a goroutine, use go f(s). This new goroutine will execute concurrently with the calling one.
    go (f "goroutine")

    // You can also start a goroutine for an anonymous function call.
    go ((lambda (msg :: string) = print msg) "going")

    // Our two function calls are running asynchronously in separate goroutines now, so execution falls through to here.
    // This Scanln code requires we press a key before the program exits.
    var input :: string
    fmt.Scanln &input
    print "done"
