package print

import (
	"bytes"
	"fmt"
	"go/format"
	"strings"

	"github.com/jBugman/fun-lang/fun"
)

// FixFormat formats valid Go code.
func FixFormat(source []byte) (string, error) {
	result, err := format.Source(source)
	return string(result), err
}

// Module prints fun.Module.
func Module(mod fun.Module) ([]byte, error) {
	var err error
	var buf bytes.Buffer
	// Package
	fmt.Fprintf(&buf, "package %s%s\n", strings.ToLower(string(mod.Name[0])), mod.Name[1:])
	// Imports
	switch len(mod.Imports) {
	case 0: // do nothing
	case 1:
		fmt.Fprint(&buf, "import ", Import(mod.Imports[0]), fun.LF)
	default:
		fmt.Fprint(&buf, "import (\n")
		for _, imp := range mod.Imports {
			fmt.Fprint(&buf, Import(imp), fun.LF)
		}
		fmt.Fprint(&buf, ")\n")
	}
	// Top-level declarations
	for _, decl := range mod.Decls {
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
		fmt.Fprint(&buf, line, fun.LF)
	}
	return buf.Bytes(), nil
}

// Import prints fun.Import.
func Import(imp fun.Import) string {
	return fmt.Sprintf("%s \"%s\"", imp.Alias, imp.Path)
}

// FuncDecl prints fun.Decl.
func FuncDecl(f fun.FuncDecl) (string, error) {
	var body string
	var err error
	switch b := f.Body.(type) {
	case fun.DoBlock:
		body = strings.Join(b.Text, fun.LF)
	case fun.SingleExprBody:
		body, err = Expression(b.Expr)
		if err != nil {
			return "", err
		}
		if f.Results.ShouldReturn() {
			body = fmt.Sprintf("%s %s", fun.RETURN, body)
		}
	default:
		return "", fmt.Errorf("body type is not supported: %s", b)
	}
	return fmt.Sprintf("func %s(%s) %s {\n%s\n}", f.Name, Parameters(f.Params), Results(f.Results), body), nil
}

// Parameters prints fun.Parameters.
func Parameters(ps fun.Parameters) string {
	ss := make([]string, len(ps))
	for i := 0; i < len(ps); i++ {
		ss[i] = fmt.Sprintf("%s %s", ps[i].Name, ps[i].Type)
	}
	return strings.Join(ss, fun.COMMA)
}

// Results prints fun.Results.
func Results(results fun.Results) string {
	rs := results.Types
	switch len(rs) {
	case 0:
		return "" // Empty result == IO ()
	case 1:
		return fmt.Sprint(rs[0])
	default:
		ss := make([]string, len(rs))
		for i := 0; i < len(rs); i++ {
			ss[i] = fmt.Sprint(rs[i])
		}
		return fun.OPENBR + strings.Join(ss, fun.COMMA) + fun.CLOSEBR
	}
}

// InfixOperation prints fun.InfixOperation.
func InfixOperation(op fun.InfixOperation) (string, error) {
	x, err := Expression(op.X)
	if err != nil {
		return "", err
	}
	y, err := Expression(op.Y)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%s %s %s", x, op.Operator, y), nil
}

// Expression dispatches printers for concrete expression types.
func Expression(e fun.Expression) (string, error) {
	switch expr := e.(type) {
	case fun.FuncApplication:
		return FuncApplication(expr)
	case fun.Literal:
		return fmt.Sprint(expr), nil
	case fun.InfixOperation:
		return InfixOperation(expr)
	case fun.Val:
		return fmt.Sprint(expr), nil
	case fun.ReturnList:
		return ReturnList(expr), nil
	default:
		return "", fmt.Errorf("NOT IMPLEMENTED %s", expr)
	}
}

// FuncApplication prints fun.FuncApplication.
func FuncApplication(fa fun.FuncApplication) (string, error) {
	var err error
	ss := make([]string, len(fa.Arguments))
	for i := 0; i < len(fa.Arguments); i++ {
		ss[i], err = Expression(fa.Arguments[i])
		if err != nil {
			return "", err
		}
	}
	return fmt.Sprintf("%s(%s)", FunctionVal(fa.Func), strings.Join(ss, fun.COMMA)), nil
}

// FunctionVal prints fun.FunctionVal.
func FunctionVal(v fun.FunctionVal) string {
	var buf bytes.Buffer
	if v.Module != "" {
		fmt.Fprint(&buf, v.Module, fun.DOT)
	}
	fmt.Fprint(&buf, v.Name)
	return buf.String()
}

// AtomicType prints fun.AtomicType.
func AtomicType(t fun.AtomicType) string {
	return string(t)
}

// ObjectType prints fun.ObjectType.
func ObjectType(t fun.ObjectType) string {
	return string(t)
}

// ListType prints fun.ListType.
func ListType(t fun.ListType) string {
	return fmt.Sprintf("[]%s", t.T)
}

// ReturnList prints multiple values to return.
func ReturnList(t fun.ReturnList) string {
	ss := make([]string, len(t))
	for i := 0; i < len(t); i++ {
		ss[i] = fmt.Sprint(t[i])
	}
	return strings.Join(ss, fun.COMMA)
}
