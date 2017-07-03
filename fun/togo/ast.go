package togo

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/format"
	"go/token"

	"github.com/jBugman/fun-lang/fun"
)

// TODO: Package

// Import converts Fun Import to Go AST.
func Import(imp fun.Import) (ast.Spec, error) {
	var alias = ident(imp.Alias)
	if imp.Alias == "" {
		alias = nil
	}
	return &ast.ImportSpec{
		// Doc     *CommentGroup // associated documentation; or nil
		Name: alias,
		Path: stringLit(imp.Path),
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
	r, err := typeList(rs)
	if err != nil {
		return nil, err
	}
	return &ast.FuncType{
		// Func    token.Pos  // position of "func" keyword (token.NoPos if there is no "func")
		Params:  p,
		Results: r,
	}, nil
}

// TODO: FuncBody
func FuncBody(x fun.FuncBody) (*ast.BlockStmt, error) {
	switch t := x.(type) {
	default:
		return nil, fmt.Errorf("not implemented FuncBody for a %#v", t)
	}
}

// Parameters converts function parameters to Go AST.
func Parameters(params []fun.Param) (*ast.FieldList, error) {
	var xs []*ast.Field
	if len(params) > 0 {
		for i := 0; i < len(params); i++ {
			x, err := Field(params[i].V)
			if err != nil {
				return nil, err
			}
			xs = append(xs, x)
		}
	}
	return &ast.FieldList{List: xs}, nil
}

// Type converts type literals to Go AST.
func Type(t fun.Type) (ast.Expr, error) {
	switch x := t.(type) {
	case fun.Atomic:
		return Atomic(x)
	case fun.Slice:
		return Slice(x)
	// TODO: add Map
	default:
		return nil, fmt.Errorf("not implemented Type for a %#v", x)
	}
}

// TODO: BinaryOp

// Expression
func Expression(expr fun.Expr) (ast.Expr, error) {
	switch t := expr.(type) {
	case fun.Literal:
		return Literal(t)
	default:
		return nil, fmt.Errorf("not implemented Expression for a %#v", t)
	}
}

// Application converts Fun Application to Go AST.
func Application(a fun.Application) (ast.Expr, error) {
	sel, err := Selector(a.Fun)
	if err != nil {
		return nil, err
	}
	exprs, err := convertExprs(a.Args)
	if err != nil {
		return nil, err
	}
	return &ast.CallExpr{
		Fun:  sel,
		Args: exprs,
		// Ellipsis token.Pos // position of "..." (token.NoPos if there is no "...")
	}, nil
}

// TODO: Selector
func Selector(x fun.Selector) (ast.Expr, error) {
	return nil, fmt.Errorf("not implemented Expression for a %#v", x)
}

// Atomic converts Fun Atomic type to Go AST.
func Atomic(t fun.Atomic) (ast.Expr, error) {
	var s = t.V
	if t == fun.CharT {
		s = "byte"
	}
	return ident(s), nil
}

// TODO: Results

// Literal converts Fun Literal to Go AST.
func Literal(o fun.Literal) (ast.Expr, error) {
	// ValuePos token.Pos   // literal position
	switch v := o.(type) {
	case fun.CharLit:
		return &ast.BasicLit{Kind: token.CHAR, Value: fmt.Sprintf("'%c'", v.V)}, nil
	case fun.StringLit:
		return stringLit(v.V), nil
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
func Slice(t fun.Slice) (*ast.ArrayType, error) {
	v, err := Type(t.V)
	return &ast.ArrayType{Elt: v}, err
}

// typeList converts function return list to Go AST.
func typeList(types []fun.Type) (*ast.FieldList, error) {
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
		Names: []*ast.Ident{varname(f.Name)},
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

func varname(x string) *ast.Ident {
	return &ast.Ident{
		Name: x,
		Obj:  &ast.Object{Kind: ast.Var, Name: x},
	}
}

func stringLit(x string) *ast.BasicLit {
	return &ast.BasicLit{Kind: token.STRING, Value: fmt.Sprintf("\"%s\"", x)}
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

// PrintAST formats node in canonical gofmt style.
func PrintAST(node interface{}) (string, error) {
	var buf bytes.Buffer
	err := format.Node(&buf, nil, node)
	return buf.String(), err
}
