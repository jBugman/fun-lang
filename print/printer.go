package print

import (
	"bytes"
	"fmt"
	"go/format"
	"strings"

	"github.com/jBugman/fun-lang/fun"
)

// FixFormat formats valid Go code
func FixFormat(source []byte) (string, error) {
	result, err := format.Source(source)
	return string(result), err
}

// Module prints fun.Module
func Module(mod fun.Module) ([]byte, error) {
	var buf bytes.Buffer
	// topLevels := make([]string, 1+1+len(mod.Decls))
	// Module name
	// topLevels[0] = fmt.Sprintf("package %s%s\n", strings.ToLower(string(mod.Name[0])), mod.Name[1:])
	fmt.Fprintf(&buf, "package %s%s\n", strings.ToLower(string(mod.Name[0])), mod.Name[1:])
	// Imports
	// imports := make([]string, len(mod.Imports))
	switch len(mod.Imports) {
	case 0: // do nothing
	case 1:
		fmt.Fprint(&buf, "import ", Import(mod.Imports[0]), fun.LF)
	default:
		fmt.Fprint(&buf, "import (\n")
		for _, imp := range mod.Imports {
			fmt.Fprint(&buf, Import(imp), fun.LF)
			// imports[i] = Import(imp)
		}
		fmt.Fprint(&buf, ")\n")
	}
	// if len(mod.Imports) > 0 {
	// 	fmt.Fprint(&buf, "import (\n")
	// 	for _, imp := range mod.Imports {
	// 		fmt.Fprintf(&buf, "%s \"%s\"\n", imp.Alias, imp.Path)
	// 		// imports[i] = Import(imp)
	// 	}
	// 	fmt.Fprint(&buf, ")\n")
	// }
	// topLevels[1] = strings.Join(imports, fun.LF) + fun.LF

	// Top-level declarations
	for _, decl := range mod.Decls {
		var line string
		switch d := decl.(type) {
		case fun.FuncDecl:
			line = FuncDecl(d)
		default:
			return nil, fmt.Errorf("unsupported type: %s", d)
		}
		fmt.Fprint(&buf, line, fun.LF)
		// topLevels[2+i] = Decl(decl)
	}

	return buf.Bytes(), nil //strings.Join(topLevels, fun.LF) + fun.LF
}

// Import prints fun.Import
func Import(imp fun.Import) string {
	return fmt.Sprintf("%s \"%s\"", imp.Alias, imp.Path)
}

// FuncDecl prints fun.Decl
func FuncDecl(imp fun.Decl) string {
	return "// FuncDecl here"
}
