package printer

/*
import (
	"bytes"
	"fmt"
	"strings"
)

const (
	lf        = "\n"
	undefined = "undefined"
	arrow     = "->"
	_io       = "IO" // TODO remove
	unit      = "()" // TODO remove
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

func (pk Package) String() string {
	topLevels := make([]string, 1+1+len(pk.TopLevels))

	// Module name
	topLevels[0] = fmt.Sprintf("module %s where%s", pk.Name, lf)

	// Imports
	imports := make([]string, len(pk.Imports))
	for i, imp := range pk.Imports {
		imports[i] = imp.String()
	}
	topLevels[1] = strings.Join(imports, lf) + lf

	// Top-level declarations
	for i, decl := range pk.TopLevels {
		topLevels[2+i] = fmt.Sprint(decl) + lf
	}

	return strings.Join(topLevels, lf)
}

func (fd FuncDecl) String() string {
	var out bytes.Buffer
	// Type signature
	fmt.Fprintf(&out, "%s :: %s", fd.Name, fd.Params)
	if len(fd.Params) > 0 {
		fmt.Fprintf(&out, " %s ", arrow)
	}
	fmt.Fprint(&out, fd.Results, lf)

	// Name and parameters
	fmt.Fprint(&out, fd.Name)
	if len(fd.Params) > 0 {
		fmt.Fprint(&out, " ", fd.Params.Names())
	}
	fmt.Fprint(&out, " = ")

	if fd.Body == nil {
		fmt.Fprint(&out, undefined)
	} else {
		fmt.Fprint(&out, fd.Body)
	}

	return out.String()
}

func (ps Parameters) String() string {
	ss := make([]string, len(ps))
	for i := 0; i < len(ps); i++ {
		ss[i] = fmt.Sprint(ps[i].Type)
	}
	const sep = " " + arrow + " "
	return strings.Join(ss, sep)
}

// Names build parameter list for binding.
func (ps Parameters) Names() string {
	ss := make([]string, len(ps))
	for i := 0; i < len(ps); i++ {
		ss[i] = string(ps[i].Name)
	}
	return strings.Join(ss, " ")
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
		result = fmt.Sprintf("(%s)", strings.Join(ss, ", "))
	}
	if !rs.Pure {
		return fmt.Sprintf("%s %s", _io, result)
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
		fmt.Fprint(&buf, " ", strings.Join(args, " "))
	}
	return buf.String()
}

func (v FunctionVal) String() string {
	var buf bytes.Buffer
	if v.Module != "" {
		fmt.Fprint(&buf, v.Module, ".")
	}
	fmt.Fprint(&buf, v.Name)
	return buf.String()
}

// Assuming non-empty body, empty do block does not really makes sense.
func (b Inline) String() string {
	buf := bytes.NewBufferString("inline\n")
	for _, line := range b.Block {
		fmt.Fprintf(buf, "    %s\n", line)
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

func (t ReturnList) String() string {
	ss := make([]string, len(t))
	for i := 0; i < len(t); i++ {
		ss[i] = fmt.Sprint(t[i])
	}
	return fmt.Sprintf("(%s)", strings.Join(ss, ", "))
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
	return unit
}
*/
