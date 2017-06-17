package fun

// NewParam creates new Parameter instance
func NewParam(name string, t AtomicType) Parameter {
	return Parameter{Name: name, Type: t}
}

// NewList creates new list type with atomic elements
func NewList(t AtomicType) ListType {
	return ListType{T: t}
}

// EmptyResults creates function results for a function which does not return anything
func EmptyResults() Results {
	return Results{
		IO:    true,
		Types: []Type{Unit},
	}
}

// SingleResult wraps single return type
func SingleResult(t Type) Results {
	return Results{
		IO:    false,
		Types: []Type{t},
	}
}

// ShouldReturn marks if function should return anything
func (rs Results) ShouldReturn() bool {
	switch len(rs.Types) {
	case 0:
		panic("zero length Results")
	case 1:
		return (rs.Types[0] != Unit) // everything except IO () should be returned
	default:
		return true
	}
}
