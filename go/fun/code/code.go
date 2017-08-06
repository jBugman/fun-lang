// Package code contains implementation details related to source code.
package code

// Pos represents position in code.
type Pos struct {
	Line int
	Col  int
}

// NewPos creates new Pos.
func NewPos(line, col int) Pos {
	return Pos{
		Line: line,
		Col:  col,
	}
}
