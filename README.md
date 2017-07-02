## Fun
### Lighter, more functional language, compiled to Go

Idea is to provide cleaner (subjective) syntax and later some code-generation features around simple constructs to allow more concise code. Also, there is an ability to use raw inline Go code for more complex cases.

⚠️ Under active development. Expect changes to everything.


    package main

    import "io"

    func inc (val int) -> int = val + 1

    func print42 = print 42

    func main = do
        var line = "Hello World!"
        fmt.Fprintln io.Discard line


Tests

    gocov test ./fun ./print ./translate ./parse/... | gocov-html > coverage.html && open coverage.html
