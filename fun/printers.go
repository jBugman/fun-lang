package fun

import (
	"bytes"
	"fmt"
	"strings"
)

func (u Undef) String() string {
	return undefined
}

func (imp Import) String() string {
	if imp.Alias == "" {
		return fmt.Sprintf("import \"%s\"", imp.Path)
	}
	return fmt.Sprintf("import \"%s\" as \"%s\"", imp.Path, imp.Alias)
}

func (mod Module) String() string {
	topLevels := make([]string, 1+1+len(mod.Decls))

	// Module name
	topLevels[0] = fmt.Sprintf("module %s where\n", mod.Name)

	// Imports
	imports := make([]string, len(mod.Imports))
	for i, imp := range mod.Imports {
		imports[i] = imp.String()
	}
	topLevels[1] = strings.Join(imports, lf) + lf

	// Top-level declarations
	for i, decl := range mod.Decls {
		topLevels[2+i] = fmt.Sprint(decl)
	}

	return strings.Join(topLevels, lf) + lf
}

func (fd FuncDecl) String() string {
	var out bytes.Buffer
	// Type signature
	fmt.Fprint(&out, fd.Name)
	fmt.Fprint(&out, typeSeparator)
	fmt.Fprint(&out, fd.Params)
	if len(fd.Params) > 0 {
		fmt.Fprint(&out, arrow)
	}
	fmt.Fprint(&out, fd.Results, lf)

	// Name and parameters
	fmt.Fprint(&out, fd.Name)
	if len(fd.Params) > 0 {
		fmt.Fprint(&out, space, fd.Params.Names())
	}
	fmt.Fprint(&out, binding)

	// TODO implement body
	if fd.Body == nil {
		fmt.Fprint(&out, undefined)
	} else {
		fmt.Fprint(&out, fd.Body)
	}

	return out.String() + lf
}

func (ps Parameters) String() string {
	ss := make([]string, len(ps))
	for i := 0; i < len(ps); i++ {
		ss[i] = string(ps[i].Type)
	}
	return strings.Join(ss, arrow)
}

// Names build parameter list for binding.
func (ps Parameters) Names() string {
	ss := make([]string, len(ps))
	for i := 0; i < len(ps); i++ {
		ss[i] = string(ps[i].Name)
	}
	return strings.Join(ss, space)
}

func (ts Results) String() string {
	switch len(ts) {
	case 0:
		return unit
	case 1:
		return string(ts[0])
	default:
		ss := make([]string, len(ts))
		for i := 0; i < len(ts); i++ {
			ss[i] = string(ts[i])
		}
		return openBracket + strings.Join(ss, comma) + closeBracket
	}
}

func (fa FuncApplication) String() string {
	var buf bytes.Buffer
	fmt.Fprint(&buf, fa.Func)
	if len(fa.Arguments) > 0 {
		args := make([]string, len(fa.Arguments))
		for i := 0; i < len(args); i++ {
			switch arg := fa.Arguments[i].(type) {
			case FuncApplication:
				args[i] = fmt.Sprintf("(%s)", arg)
			default:
				args[i] = fmt.Sprint(arg)
			}
		}
		fmt.Fprint(&buf, space, strings.Join(args, space))
	}
	return buf.String()
}

func (v FunctionVal) String() string {
	var buf bytes.Buffer
	if v.Module != "" {
		fmt.Fprint(&buf, v.Module, dot)
	}
	fmt.Fprint(&buf, v.Name)
	return buf.String()
}

// Assuming non-empty body, empty do block does not really makes sense.
func (do DoBlock) String() string {
	buf := bytes.NewBufferString(doDecl)
	for _, line := range do.Text {
		fmt.Fprint(buf, intendation, line, lf)
	}
	return strings.TrimSuffix(buf.String(), lf)
}

func (t String) String() string {
	return fmt.Sprintf(`"%s"`, string(t))
}

func (t Char) String() string {
	return fmt.Sprintf("'%s'", string(t))
}

func (op InfixOperation) String() string {
	return fmt.Sprintf("%s %s %s", op.X, op.Operator, op.Y)
}

func (t Tuple) String() string {
	ss := make([]string, len(t))
	for i := 0; i < len(t); i++ {
		ss[i] = fmt.Sprint(t[i])
	}
	return openBracket + strings.Join(ss, comma) + closeBracket
}

func (b SingleExprBody) String() string {
	return fmt.Sprint(b.Expr)
}
