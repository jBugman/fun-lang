package fun

// NewParam creates new Param instance.
func NewParam(name, t string) Param {
	return Param{VarSpec{Name: name, Type: Atomic{V: t}}}
}
