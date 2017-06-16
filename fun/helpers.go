package fun

// NewParam creates new Parameter instance
func NewParam(name string, t Type) Parameter {
	return Parameter{Name: name, Type: t}
}
