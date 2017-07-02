package togo_test

import (
	"go/ast"
	"go/token"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/jBugman/fun-lang/fun"
	"github.com/jBugman/fun-lang/fun/togo"
)

func Test_Import(t *testing.T) {
	funTree := fun.Import{Path: "fmt"}
	goTree := &ast.ImportSpec{
		Path: &ast.BasicLit{
			Kind:  token.STRING,
			Value: "fmt",
		},
	}
	result, err := togo.Import(funTree)
	if assert.NoError(t, err) {
		assert.Equal(t, goTree, result)
	}
}
