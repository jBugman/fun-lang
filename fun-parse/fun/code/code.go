// Package code contains implementation details related to source code.
package code

// Pos represents position in code.
type Pos struct {
	Line int `json:"ln"`
	Col  int `json:"col"`
}
