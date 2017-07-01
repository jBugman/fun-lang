package fun

// NewParam creates new Param instance.
func NewParam(name string, t Atomic) Param {
	return Param{VarSpec{Name: name, Type: t}}
}

// NewSlice creates new slice type with atomic elements.
func NewSlice(t Atomic) Slice {
	return Slice{T: t}
}
