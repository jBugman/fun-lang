package fun

// NewParam creates new Parameter instance
func NewParam(name string, t AtomicType) Parameter {
	return Parameter{Name: name, Type: t}
}

// NewList creates new list type with atomic elements
func NewList(t AtomicType) ListType {
	return ListType{T: t}
}
