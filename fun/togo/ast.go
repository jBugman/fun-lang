package togo

import (
	"fmt"
	"go/ast"
	"go/token"

	"go/format"
	"io"

	"github.com/jBugman/fun-lang/fun"
)

// TODO Package

// Import converts Fun Import to Go AST.
func Import(imp fun.Import) (ast.Spec, error) {
	var alias = ident(imp.Alias)
	if imp.Alias == "" {
		alias = nil
	}
	return &ast.ImportSpec{
		// Doc     *CommentGroup // associated documentation; or nil
		Name: alias,
		Path: txt(imp.Path),
		// Comment *CommentGroup // line comments; or nil
		// EndPos  token.Pos     // end of spec (overrides Path.Pos if nonzero)
	}, nil
}

// FuncDecl converts Fun FuncDecl to Go AST.
func FuncDecl(f fun.FuncDecl) (ast.Decl, error) {
	body, err := FuncBody(f.Body)
	if err != nil {
		return nil, err
	}
	t, err := funcType(f.Params, f.Results)
	if err != nil {
		return nil, err
	}
	return &ast.FuncDecl{
		// Doc  *CommentGroup // associated documentation; or nil
		// Recv *FieldList    // receiver (methods); or nil (functions)
		Name: ident(f.Name),
		Type: t, // function signature: parameters, results, and position of "func" keyword
		Body: body,
	}, nil
}

func funcType(ps []fun.Param, rs []fun.Type) (*ast.FuncType, error) {
	p, err := Parameters(ps)
	if err != nil {
		return nil, err
	}
	r, err := Results(rs)
	if err != nil {
		return nil, err
	}
	return &ast.FuncType{
		// Func    token.Pos  // position of "func" keyword (token.NoPos if there is no "func")
		Params:  p,
		Results: r,
	}, nil
}

// TODO FuncBody
func FuncBody(f fun.FuncBody) (*ast.BlockStmt, error) {
	return nil, fmt.Errorf("not implemented: %#v", f)
}

// TODO Parameters
func Parameters(xs []fun.Param) (*ast.FieldList, error) {
	return nil, fmt.Errorf("not implemented: %#v", xs)
}

// TODO Type
func Type(t fun.Type) (ast.Expr, error) {
	return nil, fmt.Errorf("not implemented: %#v", t)
}

// TODO BinaryOp

// Expression
func Expression(expr fun.Expr) (ast.Expr, error) {
	return nil, fmt.Errorf("not implemented: %#v", expr)
}

// Application converts Fun Application to Go AST.
func Application(fa fun.Application) (ast.Expr, error) {
	exprs, err := convertExprs(fa.Args)
	return &ast.CallExpr{
		Fun: ident(fa.Name.V),
		// Lparen   token.Pos // position of "("
		Args: exprs,
		// Ellipsis token.Pos // position of "..." (token.NoPos if there is no "...")
		// Rparen   token.Pos // position of ")"
	}, err
}

// TODO FuncName ?

// Atomic converts Fun Atomic type to Go AST.
func Atomic(t fun.Atomic) (ast.Expr, error) {
	var s = t.V
	if t == fun.CharT {
		s = "byte"
	}
	return ident(s), nil
}

// Literal converts Fun Literal to Go AST.
func Literal(o fun.Literal) (ast.Expr, error) {
	// ValuePos token.Pos   // literal position
	switch v := o.(type) {
	case fun.CharLit:
		return &ast.BasicLit{Kind: token.CHAR, Value: fmt.Sprintf("'%c'", v.V)}, nil
	case fun.StringLit:
		return &ast.BasicLit{Kind: token.STRING, Value: fmt.Sprintf("\"%s\"", v.V)}, nil
	case fun.BoolLit:
		return &ast.Ident{Name: fmt.Sprintf("%v", v.V)}, nil
	case fun.DoubleLit:
		return &ast.BasicLit{Kind: token.FLOAT, Value: fmt.Sprintf("%v", v.V)}, nil
	case fun.IntegerLit:
		return &ast.BasicLit{Kind: token.INT, Value: fmt.Sprintf("%v", v.V)}, nil
	case fun.HexLit:
		return &ast.BasicLit{Kind: token.INT, Value: fmt.Sprintf("%v", v.V)}, nil
	default:
		return nil, fmt.Errorf("not supported: %#v", v)
	}
}

// Slice converts Fun Slice to Go AST.
func Slice(t fun.Slice) (ast.ArrayType, error) {
	v, err := Type(t.V)
	return ast.ArrayType{Elt: v}, err
}

// Results converts function return list to Go AST.
func Results(types []fun.Type) (*ast.FieldList, error) {
	var err error
	var xs []*ast.Field
	if len(types) > 0 {
		xs := make([]*ast.Field, len(types))
		for i := 0; i < len(types); i++ {
			xs[i], err = Field(types[i].Field())
			if err != nil {
				return nil, err
			}
		}
	}
	return &ast.FieldList{
		// Opening token.Pos // position of opening parenthesis/brace, if any
		List: xs, // field list; or nil
		// Closing token.Pos // position of closing parenthesis/brace, if any
	}, nil
}

// Field represents a Field declaration list in a struct type,
// a method list in an interface type, or a parameter/result declaration
// in a signature.
func Field(f fun.Field) (*ast.Field, error) {
	t, err := Type(f.Type)
	return &ast.Field{
		// Doc     *CommentGroup // associated documentation; or nil
		Names: []*ast.Ident{ident(f.Name)},
		Type:  t,
		// Tag     *BasicLit     // field tag; or nil
		// Comment *CommentGroup // line comments; or nil
	}, err
}

/* Utils */

func ident(v string) *ast.Ident {
	return &ast.Ident{
		// NamePos token.Pos // identifier position
		Name: v,
		// Obj     *Object   // denoted object; or nil
	}
}

func txt(v string) *ast.BasicLit {
	return &ast.BasicLit{Kind: token.STRING, Value: v}
}

func convertExprs(xs []fun.Expr) ([]ast.Expr, error) {
	result := make([]ast.Expr, len(xs))
	var err error
	for i := 0; i < len(xs); i++ {
		result[i], err = Expression(xs[i])
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

/* Printer */

// PrintAST formats node in canonical gofmt style and writes the result to dst.
func PrintAST(dst io.Writer, node interface{}) error {
	return format.Node(dst, nil, node)
}
