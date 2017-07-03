package print

import (
	"bytes"
	"fmt"
	"go/format"
	"strings"

	"github.com/jBugman/fun-lang/fun"
	"github.com/jBugman/fun-lang/fun/togo"
)

// FixFormat formats valid Go code.
func FixFormat(source []byte) (string, error) {
	result, err := format.Source(bytes.TrimSpace(source))
	return string(result), err
}

// Package prints fun.Module.
func Package(pk fun.Package) ([]byte, error) {
	var err error
	var buf bytes.Buffer
	// Package
	fmt.Fprintf(&buf, "package %s%s\n", strings.ToLower(string(pk.Name[0])), pk.Name[1:])
	// Imports
	var s string
	switch len(pk.Imports) {
	case 0: // do nothing
	case 1:
		s, err = Import(pk.Imports[0])
		if err != nil {
			return nil, err
		}
		fmt.Fprintln(&buf, "import ", s)
	default:
		fmt.Fprintln(&buf, "import (")
		for _, imp := range pk.Imports {
			s, err = Import(imp)
			if err != nil {
				return nil, err
			}
			fmt.Fprintln(&buf, s)
		}
		fmt.Fprintln(&buf, ")")
	}
	// Top-level declarations
	for _, decl := range pk.TopLevels {
		var line string
		switch d := decl.(type) {
		case fun.FuncDecl:
			line, err = FuncDecl(d)
			if err != nil {
				return nil, err
			}
		default:
			return nil, fmt.Errorf("unsupported type: %s", d)
		}
		fmt.Fprintln(&buf, line)
	}
	return buf.Bytes(), nil
}

// Import prints fun.Import.
func Import(x fun.Import) (string, error) {
	node, err := togo.Import(x)
	if err != nil {
		return "", err
	}
	return togo.PrintAST(node)
}

// FuncDecl prints fun.Decl.
func FuncDecl(f fun.FuncDecl) (string, error) {
	var body string
	var err error
	switch b := f.Body.(type) {
	case fun.Inline:
		body = strings.Join(b.Block, "\n")
	case fun.Single:
		body, err = Expression(b.Expr)
		if err != nil {
			return "", err
		}
		if len(f.Results) > 0 {
			body = fmt.Sprintf("return %s", body)
		}
	default:
		return "", fmt.Errorf("body type is not supported: %s", b)
	}
	var params, results string
	params, err = Parameters(f.Params)
	if err != nil {
		return "", err
	}
	results, err = typeSlice(f.Results)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("func %s(%s) %s {\n%s\n}", f.Name, params, results, body), nil
}

// Parameters prints fun.Parameters.
func Parameters(ps []fun.Param) (string, error) {
	ss := make([]string, len(ps))
	for i := 0; i < len(ps); i++ {
		p, err := Type(ps[i].V.Type)
		if err != nil {
			return "", err
		}
		ss[i] = fmt.Sprintf("%s %s", ps[i].V.Name, p)
	}
	return strings.Join(ss, ", "), nil
}

func typeSlice(xs []fun.Type) (string, error) {
	n := len(xs)
	switch n {
	case 0:
		return "", nil
	case 1:
		return Type(xs[0])
	default:
		var err error
		ss := make([]string, n)
		for i := 0; i < n; i++ {
			ss[i], err = Type(xs[i])
			if err != nil {
				return "", err
			}
		}
		return fmt.Sprintf("(%s)", strings.Join(ss, ", ")), nil
	}
}

// Type prints instances of fun.Type interface
func Type(x fun.Type) (string, error) {
	node, err := togo.Type(x)
	if err != nil {
		return "", err
	}
	return togo.PrintAST(node)
}

// BinaryOp prints fun.BinaryOp.
func BinaryOp(op fun.BinaryOp) (string, error) {
	x, err := Expression(op.X)
	if err != nil {
		return "", err
	}
	y, err := Expression(op.Y)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%s %s %s", x, op.Op, y), nil
}

// Expression dispatches printers for concrete expression types.
func Expression(e fun.Expr) (string, error) {
	switch expr := e.(type) {
	case fun.Application:
		return Application(expr)
	case fun.Literal:
		return Literal(expr)
	case fun.BinaryOp:
		return BinaryOp(expr)
	case fun.Var:
		return fmt.Sprint(expr), nil
	case fun.Results:
		return Results(expr)
	default:
		return "", fmt.Errorf("NOT IMPLEMENTED %s", expr)
	}
}

// Application prints fun.Application.
func Application(x fun.Application) (string, error) {
	node, err := togo.Application(x)
	if err != nil {
		return "", err
	}
	return togo.PrintAST(node)
}

// Atomic prints fun.Atomic.
func Atomic(x fun.Atomic) (string, error) {
	node, err := togo.Atomic(x)
	if err != nil {
		return "", err
	}
	return togo.PrintAST(node)
}

// Literal prints fun.Literal.
func Literal(x fun.Literal) (string, error) {
	node, err := togo.Literal(x)
	if err != nil {
		return "", err
	}
	return togo.PrintAST(node)
}

// Slice prints fun.Slice.
func Slice(x fun.Slice) (string, error) {
	node, err := togo.Slice(x)
	if err != nil {
		return "", err
	}
	return togo.PrintAST(node)
}

// Results prints return arguments to return.
func Results(xs fun.Results) (string, error) {
	n := len(xs)
	var err error
	ss := make([]string, n)
	for i := 0; i < n; i++ {
		ss[i], err = Expression(xs[i])
		if err != nil {
			return "", err
		}
	}
	return strings.Join(ss, ", "), nil
}
