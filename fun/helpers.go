package fun

// NewParam creates new Parameter instance.
func NewParam(name string, t AtomicType) Parameter {
	return Parameter{Name: name, Type: t}
}

// NewList creates new list type with atomic elements.
func NewList(t AtomicType) ListType {
	return ListType{T: t}
}

// SingleResult wraps single return type.
func SingleResult(t Type) Results {
	return Results{Types: []Type{t}}
}

// ShouldReturn marks if function should return anything.
func (rs Results) ShouldReturn() bool {
	switch len(rs.Types) {
	case 0:
		return false
	case 1:
		return (rs.Types[0] != Unit) // Looks like Unit should not be returned explicitly
	default:
		return true
	}
}
