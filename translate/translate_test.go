package translate_test

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/jBugman/fun-lang/fun"
	"github.com/jBugman/fun-lang/translate"
)

// Verifying how Go handles positions in parse errors
func TestErrorLocation_99bottles(t *testing.T) {
	fset := token.NewFileSet()
	_, err := parser.ParseFile(fset, "test.go", "package main\n99bottles", 0)
	assert.EqualError(t, err, "test.go:2:1: expected declaration, found 'INT' 99")
}

func TestFun_Package(t *testing.T) {
	const fullSource = `
	package main

	import (
		"fmt"
		"io"
	)

	func inc(val int) int {
		return val + 1
	}

	func print42() {
		fmt.Println(42)
	}

	func main() {
		line := "Hello World!"
		fmt.Fprintln(io.Discard, line)
	}
	`
	sample := fun.Package{
		Name:    "main",
		Imports: []fun.Import{{Path: "fmt"}, {Path: "io"}},
		TopLevels: []fun.TopLevel{
			fun.FuncDecl{
				Name:    "inc",
				Params:  fun.Parameters{{Name: "val", Type: fun.IntT}},
				Results: fun.Results{Types: []fun.Type{fun.IntT}},
				Body: fun.SingleExprBody{
					Expr: fun.InfixOperation{
						X:        fun.Val("val"),
						Operator: fun.Operator("+"),
						Y:        fun.Int("1"),
					}}},
			fun.FuncDecl{
				Name: "print42",
				Body: fun.SingleExprBody{
					Expr: fun.FuncApplication{
						Func:      fun.FunctionVal{Module: "fmt", Name: "Println"},
						Arguments: []fun.Expression{fun.Int("42")},
					}}},
			fun.FuncDecl{
				Name: "main",
				Body: fun.Inline{Block: []string{
					`line := "Hello World!"`,
					`fmt.Fprintln(io.Discard, line)`,
				}},
			}}}
	fset := token.NewFileSet()
	tree, _ := parser.ParseFile(fset, "source.go", fullSource, 0)
	result, err := translate.NewFun(fset).Package(tree)
	assert.NoError(t, err)
	assert.Equal(t, sample, result)
}

func TestFun_Import(t *testing.T) {
	fset := token.NewFileSet()
	tree := &ast.ImportSpec{
		Path: &ast.BasicLit{
			Kind:  token.STRING,
			Value: "fmt",
		},
	}
	sample := fun.Import{Path: "fmt"}
	result, err := translate.NewFun(fset).Import(tree)
	if assert.NoError(t, err) {
		assert.Equal(t, sample, result)
	}
}

func TestFun_Expression_selector(t *testing.T) {
	fset := token.NewFileSet()
	tree := &ast.SelectorExpr{
		X:   &ast.Ident{Name: "fmt"},
		Sel: &ast.Ident{Name: "Println"},
	}
	sample := fun.FunctionVal{
		Module: "fmt",
		Name:   "Println",
	}
	result, err := translate.NewFun(fset).Expression(tree)
	if assert.NoError(t, err) {
		assert.Equal(t, sample, result)
	}
}

func TestFun_Expression_binary(t *testing.T) {
	fset := token.NewFileSet()
	tree := &ast.BinaryExpr{
		X: &ast.Ident{
			Name: "val",
			Obj:  &ast.Object{Kind: ast.Var, Name: "val"},
		},
		Op: token.ADD,
		Y:  &ast.BasicLit{Kind: token.INT, Value: "1"},
	}
	result, err := translate.NewFun(fset).Expression(tree)
	sample := fun.InfixOperation{
		X:        fun.Val("val"),
		Operator: fun.Operator("+"),
		Y:        fun.Int("1"),
	}
	if assert.NoError(t, err) {
		assert.Equal(t, sample, result)
	}
}

func TestFun_Statement_return(t *testing.T) {
	fset := token.NewFileSet()
	tree := &ast.ReturnStmt{
		Results: []ast.Expr{
			&ast.BasicLit{Kind: token.INT, Value: "42"},
		},
	}
	result, err := translate.NewFun(fset).Statement(tree)
	if assert.NoError(t, err) {
		assert.Equal(t, "42", fmt.Sprint(result))
	}
}

func TestFun_Statement_tuple(t *testing.T) {
	fset := token.NewFileSet()
	expr, err := parser.ParseExpr("func() {return 'a', 9.99}")
	returnExpr := expr.(*ast.FuncLit).Body.List[0]
	result, err := translate.NewFun(fset).Statement(returnExpr)
	sample := fun.ReturnList{
		fun.Char("a"),
		fun.Double("9.99"),
	}
	if assert.NoError(t, err) {
		assert.Equal(t, sample, result)
	}
}

func TestFun_Expression_brokenLiteral_testError(t *testing.T) {
	fset := token.NewFileSet()
	tree := &ast.BasicLit{
		Kind:  token.BREAK,
		Value: "SNAKE!",
	}
	_, err := translate.NewFun(fset).Expression(tree)
	assert.EqualError(t, err, ex(`
	unexpected literal type:
	     0  *ast.BasicLit {
	     1  .  ValuePos: -
	     2  .  Kind: break
	     3  .  Value: "SNAKE!"
	     4  }
	`))
}

func ex(source string) string {
	lines := strings.Split(strings.TrimSpace(source), "\n")
	for i := 0; i < len(lines); i++ {
		lines[i] = strings.TrimPrefix(lines[i], "\t")
	}
	return strings.Join(lines, "\n") + "\n"
}

func TestFun_Module_unsupportedNakedReturn(t *testing.T) {
	source := `
	package foo

	func noop() {
		return
	}
	`
	fset := token.NewFileSet()
	tree, err := parser.ParseFile(fset, "", source, 0)
	assert.NoError(t, err)
	_, err = translate.NewFun(fset).Package(tree)
	assert.EqualError(t, err, ex(`
	result list of zero length is not supported:
	     0  *ast.ReturnStmt {
	     1  .  Return: 5:3
	     2  }
	`))
}

func TestFun_Module_unsupportedForwardDeclare(t *testing.T) {
	source := `
	package foo

	func forwardDeclared() bool
	`
	fset := token.NewFileSet()
	tree, err := parser.ParseFile(fset, "", source, 0)
	assert.NoError(t, err)
	_, err = translate.NewFun(fset).Package(tree)
	assert.Error(t, err)
}
