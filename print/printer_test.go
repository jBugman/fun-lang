package print_test

import (
	"fmt"
	"testing"

	"github.com/jBugman/fun-lang/fun"
	"github.com/jBugman/fun-lang/print"
	"github.com/stretchr/testify/assert"
)

func TestFixFormat(t *testing.T) {
	src := `func f ( 
		
	)   {  return 1   + 2
		}
			`
	result, err := print.FixFormat([]byte(src))
	assert.NoError(t, err)
	assert.Equal(t, `func f() {
	return 1 + 2
}`, result)
}

func ex(source string) string {
	r, _ := print.FixFormat([]byte(source))
	return r
}

func TestPackage(t *testing.T) {
	tree := fun.Package{
		Name: "Main",
		Imports: []fun.Import{
			{Path: "fmt"},
		},
		TopLevels: []fun.TopLevel{
			fun.FuncDecl{
				Name: "main",
				Body: fun.SingleExprBody{
					Expr: fun.FuncApplication{
						Func: fun.FunctionVal{
							Module: "fmt",
							Name:   "Println",
						},
						Arguments: []fun.Expression{
							fun.String("Hello World!"),
						},
					},
				},
			},
		},
	}
	source, err := print.Package(tree)
	assert.NoError(t, err)
	result, err := print.FixFormat(source)
	assert.NoError(t, err)
	assert.Equal(t, ex(`
	package main
	
	import "fmt"
	
	func main() {
		fmt.Println("Hello World!")
	}
	`), fmt.Sprint(string(result)))
}

func TestPackage_multiImports(t *testing.T) {
	tree := fun.Package{
		Name: "Test",
		Imports: []fun.Import{
			{Path: "log"},
			{Path: "./log", Alias: "myLog"},
		},
	}
	source, err := print.Package(tree)
	assert.NoError(t, err)
	result, err := print.FixFormat(source)
	assert.NoError(t, err)
	assert.Equal(t, ex(`
	package test
	
	import (
		myLog "./log"
		"log"
	)
	`), fmt.Sprint(result))
}

func TestFuncDecl_infixReturn(t *testing.T) {
	tree := fun.FuncDecl{
		Name:    "myFunc",
		Results: fun.SingleResult(fun.IntT),
		Body: fun.SingleExprBody{
			Expr: fun.InfixOperation{
				X:        fun.Val("x"),
				Operator: fun.Operator("+"),
				Y:        fun.Int("2"),
			}},
	}
	source, err := print.FuncDecl(tree)
	assert.NoError(t, err)
	result, err := print.FixFormat([]byte(source))
	assert.NoError(t, err)
	assert.Equal(t, ex(`
	func myFunc() int {
		return x + 2
	}
	`), fmt.Sprint(result))
}

func TestFuncDecl_doBlock_multiline(t *testing.T) {
	tree := fun.FuncDecl{
		Name:   "printHash",
		Params: fun.Parameters{fun.NewParam("str", "string")},
		Body: fun.Inline{Block: []string{
			`h := md5.New()`,
			`io.WriteString(h, str)`,
			`fmt.Printf("%x", h.Sum(nil))`,
		}},
	}
	source, err := print.FuncDecl(tree)
	assert.NoError(t, err)
	result, err := print.FixFormat([]byte(source))
	assert.NoError(t, err)
	assert.Equal(t, ex(`
	func printHash(str string) {
		h := md5.New()
		io.WriteString(h, str)
		fmt.Printf("%x", h.Sum(nil))
	}
	`), fmt.Sprint(result))
}

func TestFuncDecl_multiReturn(t *testing.T) {
	tree := fun.FuncDecl{
		Name:   "swap",
		Params: fun.Parameters{fun.NewParam("x", "int"), fun.NewParam("y", "int")},
		Results: fun.Results{
			Pure:  true,
			Types: []fun.Type{fun.IntT, fun.IntT},
		},
		Body: fun.SingleExprBody{
			Expr: fun.ReturnList{
				fun.Val("y"),
				fun.Val("x"),
			},
		},
	}
	source, err := print.FuncDecl(tree)
	assert.NoError(t, err)
	result, err := print.FixFormat([]byte(source))
	assert.NoError(t, err)
	assert.Equal(t, ex(`
	func swap(x int, y int) (int, int) {
		return y, x
	}
	`), fmt.Sprint(result))
}

func TestFuncDecl_charsAsBytes(t *testing.T) {
	tree := fun.FuncDecl{
		Name:   "fun",
		Params: fun.Parameters{fun.NewParam("x", "char")},
		Results: fun.Results{
			Pure: true,
			Types: []fun.Type{
				fun.AtomicType("char"),
				fun.StringT,
				fun.ObjectType("apples"),
			},
		},
		Body: fun.SingleExprBody{
			Expr: fun.ReturnList{
				fun.Char("a"),
				fun.String("word"),
				fun.Val("aapl"),
			},
		},
	}
	source, err := print.FuncDecl(tree)
	assert.NoError(t, err)
	result, err := print.FixFormat([]byte(source))
	assert.NoError(t, err)
	assert.Equal(t, ex(`
	func fun(x byte) (byte, string, apples) {
		return 'a', "word", aapl
	}
	`), fmt.Sprint(result))
}

func TestFuncDecl_listTypeArg(t *testing.T) {
	tree := fun.FuncDecl{
		Name: "size",
		Params: fun.Parameters{
			fun.Parameter{
				Name: "xs",
				Type: fun.ListType{T: fun.IntT},
			},
		},
		Results: fun.SingleResult(fun.IntT),
		Body: fun.SingleExprBody{
			Expr: fun.FuncApplication{
				Func:      fun.FunctionVal{Name: "len"},
				Arguments: []fun.Expression{fun.Val("xs")},
			},
		},
	}
	source, err := print.FuncDecl(tree)
	assert.NoError(t, err)
	result, err := print.FixFormat([]byte(source))
	assert.NoError(t, err)
	assert.Equal(t, ex(`
	func size(xs []int) int {
		return len(xs)
	}
	`), fmt.Sprint(result))
}
