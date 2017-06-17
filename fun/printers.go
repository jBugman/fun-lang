package fun

import (
	"bytes"
	"fmt"
	"strings"
)

func (u Undef) String() string {
	return UNDEFINED
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
	topLevels[1] = strings.Join(imports, LF) + LF

	// Top-level declarations
	for i, decl := range mod.Decls {
		topLevels[2+i] = fmt.Sprint(decl)
	}

	return strings.Join(topLevels, LF) + LF
}

func (fd FuncDecl) String() string {
	var out bytes.Buffer
	// Type signature
	fmt.Fprint(&out, fd.Name)
	fmt.Fprint(&out, COLONS)
	fmt.Fprint(&out, fd.Params)
	if len(fd.Params) > 0 {
		fmt.Fprint(&out, ARROW)
	}
	fmt.Fprint(&out, fd.Results, LF)

	// Name and parameters
	fmt.Fprint(&out, fd.Name)
	if len(fd.Params) > 0 {
		fmt.Fprint(&out, SPACE, fd.Params.Names())
	}
	fmt.Fprint(&out, BINDING)

	// TODO implement body
	if fd.Body == nil {
		fmt.Fprint(&out, UNDEFINED)
	} else {
		fmt.Fprint(&out, fd.Body)
	}

	return out.String() + LF
}

func (ps Parameters) String() string {
	ss := make([]string, len(ps))
	for i := 0; i < len(ps); i++ {
		ss[i] = fmt.Sprint(ps[i].Type)
	}
	return strings.Join(ss, ARROW)
}

// Names build parameter list for binding.
func (ps Parameters) Names() string {
	ss := make([]string, len(ps))
	for i := 0; i < len(ps); i++ {
		ss[i] = string(ps[i].Name)
	}
	return strings.Join(ss, SPACE)
}

func (rs Results) String() string {
	var result string
	switch len(rs.Types) {
	case 0:
		result = fmt.Sprint(Unit)
	case 1:
		result = fmt.Sprint(rs.Types[0])
	default:
		ss := make([]string, len(rs.Types))
		for i := 0; i < len(rs.Types); i++ {
			ss[i] = fmt.Sprint(rs.Types[i])
		}
		result = OPENBR + strings.Join(ss, COMMA) + CLOSEBR
	}
	if !rs.Pure {
		return fmt.Sprintf("%s %s", IO, result)
	}
	return result
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
		fmt.Fprint(&buf, SPACE, strings.Join(args, SPACE))
	}
	return buf.String()
}

func (v FunctionVal) String() string {
	var buf bytes.Buffer
	if v.Module != "" {
		fmt.Fprint(&buf, v.Module, DOT)
	}
	fmt.Fprint(&buf, v.Name)
	return buf.String()
}

// Assuming non-empty body, empty do block does not really makes sense.
func (do DoBlock) String() string {
	buf := bytes.NewBufferString(DO)
	for _, line := range do.Text {
		fmt.Fprint(buf, INTENDATION, line, LF)
	}
	return strings.TrimSuffix(buf.String(), LF)
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
	return OPENBR + strings.Join(ss, COMMA) + CLOSEBR
}

func (b SingleExprBody) String() string {
	return fmt.Sprint(b.Expr)
}

func (t AtomicType) String() string {
	return string(t)
}

func (t ObjectType) String() string {
	return string(t)
}

func (t ListType) String() string {
	return fmt.Sprintf("[%s]", t.T)
}

func (t UnitType) String() string {
	return UNIT
}
