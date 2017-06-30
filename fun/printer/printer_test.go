package printer_test

/*
import (
	"fmt"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/jBugman/fun-lang/fun"
)

func TestImport_String_single(t *testing.T) {
	tree := fun.Import{Path: "github.com/awesome/lib/fmt", Alias: "awfmt"}
	assert.Equal(t, `import "github.com/awesome/lib/fmt" as "awfmt"`, fmt.Sprint(tree))
}

func TestParameters_String_single(t *testing.T) {
	tree := fun.Parameters{{
		Name: "s",
		Type: fun.AtomicType("string"),
	}}
	assert.Equal(t, "string", fmt.Sprint(tree))
}

func TestParameters_String_two(t *testing.T) {
	tree := fun.Parameters{fun.NewParam("x", "int"), fun.NewParam("y", "int")}
	assert.Equal(t, "int -> int", fmt.Sprint(tree))
}

func TestParameters_String_zero(t *testing.T) {
	tree := fun.Parameters{}
	assert.Empty(t, fmt.Sprint(tree))
}

func TestResults_String_one(t *testing.T) {
	tree := fun.Results{
		Pure:  true,
		Types: []fun.Type{fun.ObjectType("io.Writer")},
	}
	assert.Equal(t, "io.Writer", fmt.Sprint(tree))
}

func TreeResults_String_pureTwo(t *testing.T) {
	tree := fun.Results{
		Pure: true,
		Types: []fun.Type{
			fun.AtomicType("int"),
			fun.AtomicType("error"),
		},
	}
	assert.Equal(t, "(int, error)", fmt.Sprint(tree))
}
func TestResults_String_zero(t *testing.T) {
	tree := fun.Results{}
	assert.Equal(t, "IO ()", fmt.Sprint(tree))
}

func ExampleFuncDecl_String_unit_implicit_undefined() {
	fn := fun.FuncDecl{Name: "main"}
	fmt.Println(fn)
	// Output:
	// main :: IO ()
	// main = undefined
}

func ExampleFuncDecl_String_unit_explicit_undefined() {
	fn := fun.FuncDecl{
		Name:   "newFunc",
		Params: fun.Parameters{fun.NewParam("launchMissles", "bool")},
		Body:   fun.Undefined,
	}
	fmt.Println(fn)
	// Output:
	// newFunc :: bool -> IO ()
	// newFunc launchMissles = undefined
}

func ExampleFuncDecl_String_int_unit_undefined() {
	fn := fun.FuncDecl{
		Name:   "print",
		Params: fun.Parameters{{Name: "n", Type: fun.AtomicType("int")}},
	}
	fmt.Println(fn)
	// Output:
	// print :: int -> IO ()
	// print n = undefined
}

func ExampleFuncDecl_String_head_undefined() {
	fn := fun.FuncDecl{
		Name:   "head",
		Params: fun.Parameters{{Name: "xs", Type: fun.NewList("int")}},
		Results: fun.Results{Types: []fun.Type{
			fun.AtomicType("int"),
			fun.AtomicType("error"),
		}},
	}
	fmt.Println(fn)
	// Output:
	// head :: [int] -> IO (int, error)
	// head xs = undefined
}

func ExampleFuncDecl_String_params() {
	fn := fun.FuncDecl{
		Name:    "enumFromTo",
		Params:  fun.Parameters{fun.NewParam("x", "a"), fun.NewParam("y", "a")},
		Results: fun.SingleResult(fun.NewList("a")),
	}
	fmt.Println(fn)
	// Output:
	// enumFromTo :: a -> a -> IO [a]
	// enumFromTo x y = undefined
}

func ExampleCreateAST() {
	pack := fun.Package{
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
						Arguments: []fun.Expression{fun.String("Hello World!")},
					},
				},
			},
		},
	}
	fmt.Print(pack)
	// Output:
	// module Main where
	//
	// import "fmt"
	//
	// main :: IO ()
	// main = fmt.Println "Hello World!"
}

func TestFuncApplication_String_local(t *testing.T) {
	tree := fun.FuncApplication{
		Func:      fun.FunctionVal{Name: "sum"},
		Arguments: []fun.Expression{fun.Int("1"), fun.Int("2")},
	}
	assert.Equal(t, "sum 1 2", fmt.Sprint(tree))
}

func TestFuncApplication_String_fmtPrintln(t *testing.T) {
	tree := fun.FuncApplication{
		Func: fun.FunctionVal{
			Name:   "Println",
			Module: "fmt",
		},
		Arguments: []fun.Expression{fun.Float("4.2")},
	}
	assert.Equal(t, "fmt.Println 4.2", fmt.Sprint(tree))
}

func TestFuncApplication_String_nestedFunction(t *testing.T) {
	tree := fun.FuncApplication{
		Func: fun.FunctionVal{Name: "take"},
		Arguments: []fun.Expression{
			fun.Int("3"),
			fun.FuncApplication{
				Func:      fun.FunctionVal{Name: "reverse"},
				Arguments: []fun.Expression{fun.Val("xs")},
			},
		},
	}
	assert.Equal(t, "take 3 (reverse xs)", fmt.Sprint(tree))
}

func ExampleFuncDecl_String_singleExprBody() {
	fn := fun.FuncDecl{
		Name:   "printInt",
		Params: fun.Parameters{fun.NewParam("x", "int")},
		Body: fun.SingleExprBody{
			Expr: fun.FuncApplication{
				Func: fun.FunctionVal{
					Name:   "Println",
					Module: "fmt",
				},
				Arguments: []fun.Expression{fun.Val("x")},
			},
		},
	}
	fmt.Println(fn)
	// Output:
	// printInt :: int -> IO ()
	// printInt x = fmt.Println x
}

func ExampleDoBlock_String_oneLine() {
	fn := fun.Inline{Block: []string{`fmt.Fprintf(&b, "world!")`}}
	fmt.Println(fn)
	// Output:
	// inline
	//     fmt.Fprintf(&b, "world!")
}

func ExampleFuncDecl_String_doBlock_multiline() {
	fn := fun.FuncDecl{
		Name:   "printHash",
		Params: fun.Parameters{fun.NewParam("str", "string")},
		Body: fun.Inline{Block: []string{
			`h := md5.New()`,
			`io.WriteString(h, str)`,
			`fmt.Printf("%x", h.Sum(nil))`,
		}},
	}
	fmt.Println(fn)
	// Output:
	// printHash :: string -> IO ()
	// printHash str = inline
	//     h := md5.New()
	//     io.WriteString(h, str)
	//     fmt.Printf("%x", h.Sum(nil))
}

func TestInfixOperation_String(t *testing.T) {
	tree := fun.InfixOperation{
		X:        fun.Val("x"),
		Operator: fun.Operator("+"),
		Y:        fun.Int("2"),
	}
	assert.Equal(t, "x + 2", fmt.Sprint(tree))
}

func TestReturnList_String(t *testing.T) {
	tree := fun.ReturnList{
		fun.Val("operator"),
		fun.String("plus"),
	}
	assert.Equal(t, `(operator, "plus")`, fmt.Sprint(tree))
}

func TestResults_ShouldReturn_empty(t *testing.T) {
	r := fun.Results{}
	assert.False(t, r.ShouldReturn())
}

func TestResults_ShouldReturn_single(t *testing.T) {
	r := fun.SingleResult(fun.AtomicType("int"))
	assert.True(t, r.ShouldReturn())
}

func TestResults_ShouldReturn_tuple(t *testing.T) {
	r := fun.Results{Types: []fun.Type{
		fun.IntT, fun.ObjectType("foo"), fun.StringT,
	}}
	assert.True(t, r.ShouldReturn())
}

func ExampleFuncDecl_multipleReturnValues() {
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
	fmt.Print(tree)
	// Output:
	// fun :: char -> (char, string, apples)
	// fun x = ('a', "word", aapl)
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
		Results: fun.Results{
			Pure:  true,
			Types: []fun.Type{fun.IntT},
		},
		Body: fun.SingleExprBody{
			Expr: fun.FuncApplication{
				Func:      fun.FunctionVal{Name: "len"},
				Arguments: []fun.Expression{fun.Val("xs")},
			},
		},
	}
	assert.Equal(t, ex(`
	size :: [int] -> int
	size xs = len xs
	`), fmt.Sprint(tree))
}

func ex(source string) string {
	lines := strings.Split(strings.TrimSpace(source), "\n")
	for i := 0; i < len(lines); i++ {
		lines[i] = strings.TrimPrefix(lines[i], "\t")
	}
	return strings.Join(lines, "\n")
}
*/
