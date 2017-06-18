## Fun
### Haskell-flavoured Go syntax sugar

Idea is to provide cleaner (subjective) syntax and later some code-generation features around simple constructs to allow more concise code. Also, there is an ability to use raw inline Go code for more complex cases.

    module Main where

    import "fmt"
    import "io"

    inc :: int -> int
    inc val = val + 1

    print42 :: IO ()
    print42 = fmt.Println 42

    main :: IO ()
    main = do
        line := "Hello World!"
        fmt.Fprintln(io.Discard, line)

Package purpose

    /fun        Language definitions
    /translate  Translation from Go to Fun
    /parse      Parsing Fun source code
    /printer    Translation from Fun to Go

Tests

    gocov test ./fun ./print ./translate ./parse/scanner | gocov-html > coverage.html && open coverage.html
