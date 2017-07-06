package main

import "C"
import (
	"fmt"
	"go/format"
)

func main() {}

//export gofmt
func gofmt(src *C.char) *C.char {
	txt := C.GoString(src)

	result, err := format.Source([]byte(txt))
	if err != nil {
		return C.CString(fmt.Sprintf("!ERR: %s", err))
	}

	return C.CString(string(result))
}
