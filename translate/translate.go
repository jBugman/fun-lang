// Package translate provides methods for convert Go AST to Fun AST.
package translate

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/printer"
	"go/token"
	"strings"

	"github.com/jBugman/fun-lang/fun"
)

// FromFile converts Go ast.File to Fun Module.
func FromFile(fset *token.FileSet, src *ast.File) (fun.Module, error) {
	var module fun.Module
	// Module name
	module.Name = strings.Title(identToString(src.Name))
	// Imports
	for _, imp := range src.Imports {
		funImp, err := Import(fset, imp)
		if err != nil {
			return module, err
		}
		module.Imports = append(module.Imports, funImp)
	}
	// Top-level declarations
	for _, gd := range src.Decls {
		switch d := gd.(type) {
		case *ast.FuncDecl:
			fn, err := Function(fset, d)
			if err != nil {
				return module, err
			}
			module.Decls = append(module.Decls, fn)
		}
	}
	return module, nil
}

// Import converts Go import to Fun Import.
func Import(fset *token.FileSet, imp *ast.ImportSpec) (fun.Import, error) {
	var result fun.Import
	s, ok := litToExpression(imp.Path).(fun.String)
	if !ok {
		return result, errorWithAST("not a string or char literal", imp.Path, fset)
	}
	result.Path = string(s)
	// TODO aliases
	return result, nil
}

// Function converts Go function declaration to the Fun one.
func Function(fset *token.FileSet, fd *ast.FuncDecl) (fun.FuncDecl, error) {
	// Name
	fn := fun.FuncDecl{Name: identToString(fd.Name)}
	// Parameters
	if fd.Type.Params.List != nil {
		for _, p := range fd.Type.Params.List {
			tp := identToString(p.Type.(*ast.Ident))
			for _, n := range p.Names {
				fn.Params = append(fn.Params, fun.Parameter{Name: identToString(n), Type: fun.Type(tp)})
			}
		}
	}
	// Results
	if fd.Type.Results != nil {
		for _, p := range fd.Type.Results.List {
			tp := identToString(p.Type.(*ast.Ident))
			fn.Results = append(fn.Results, fun.Type(tp))
		}
	}
	// Body
	if fd.Body == nil {
		return fn, errorWithAST("empty function body is not supported", fd, fset)
	}
	if len(fd.Body.List) == 1 {
		// Convert to FuncApplication
		stmt, err := Statement(fset, fd.Body.List[0])
		if err != nil {
			return fn, err
		}
		fn.Body = fun.SingleExprBody{Expr: stmt}
	} else {
		// Convert to Fun DoBlock
		db := fun.DoBlock{}
		for _, stmt := range fd.Body.List {
			var buf bytes.Buffer
			printer.Fprint(&buf, fset, stmt)
			db.Text = append(db.Text, buf.String())
		}
		fn.Body = db
	}

	return fn, nil
}

// Statement converts Go statement to a corresponding Fun Expression depending on type
func Statement(fset *token.FileSet, stmt ast.Stmt) (fun.Expression, error) {
	switch st := stmt.(type) {
	case *ast.ReturnStmt:
		lr := len(st.Results)
		switch lr {
		case 0:
			return nil, errorWithAST("result list of zero length is not supported", st, fset)
		case 1:
			// Single expression
			result, err := Expression(fset, st.Results[0])
			if err != nil {
				return nil, err
			}
			return result, nil
		default:
			// Tuple
			result := make(fun.Tuple, lr)
			for i := 0; i < lr; i++ {
				expr, err := Expression(fset, st.Results[i])
				if err != nil {
					return nil, err
				}
				result = append(result, expr)
			}
			return result, nil
		}
	case *ast.ExprStmt:
		result, err := Expression(fset, st.X)
		if err != nil {
			return nil, err
		}
		return result, nil
	default:
		return nil, errorWithAST("ast.Stmt type not supported", st, fset)
	}
}

// Expression converts Go expression to a Fun one.
func Expression(fset *token.FileSet, expr ast.Expr) (fun.Expression, error) {
	switch ex := expr.(type) {
	// case *ast.BinaryExpr:
	// 	result := fun.InfixOperation{}
	// 	return result, nil
	case *ast.SelectorExpr:
		result := fun.FunctionVal{Name: identToString(ex.Sel)}
		switch x := ex.X.(type) {
		case *ast.Ident:
			result.Module = identToString(x)
		default:
			return nil, errorWithAST("argument type not supported", x, fset)
		}
		return result, nil
	case *ast.CallExpr:
		e, err := Expression(fset, ex.Fun)
		if err != nil {
			return nil, err
		}
		funcVal, ok := e.(fun.FunctionVal)
		if !ok {
			return nil, errorWithAST("expected FunctionVal but got", e, fset)
		}
		result := fun.FuncApplication{Func: funcVal}
		var arg fun.Expression
		for _, ea := range ex.Args {
			switch a := ea.(type) {
			case *ast.BasicLit:
				arg = litToExpression(a)
			default:
				return nil, errorWithAST("argument type not supported", ea, fset)
			}
			result.Arguments = append(result.Arguments, arg)
		}
		return result, nil
	default:
		return nil, errorWithAST("Expr type not supported", ex, fset)
	}
}

func errorWithAST(message string, obj interface{}, fset *token.FileSet) error {
	var buf bytes.Buffer
	ast.Fprint(&buf, fset, obj, ast.NotNilFilter)
	return fmt.Errorf("%s:\n%s", message, buf.String())
}

func litToExpression(lit *ast.BasicLit) fun.Expression {
	switch lit.Kind {
	case token.INT:
		return fun.Int(lit.Value)
	case token.FLOAT:
		return fun.Double(lit.Value)
	case token.STRING:
		return fun.String(strings.Trim(lit.Value, `"`))
	case token.CHAR:
		return fun.Char(strings.Trim(lit.Value, "'"))
	case token.IMAG:
		return fun.Imaginary(lit.Value)
	default:
		panic(fmt.Sprintf("unexpected type: %v", lit))
	}
}

func identToString(ident *ast.Ident) string {
	return ident.Name
}
