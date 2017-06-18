package tokens

// Token represents valid Fun token
type Token string

// Possible Token values
const (
	// Special tokens
	ILLEGAL Token = "#ILLEGAL"
	EOF     Token = "EOF"
	WS      Token = "WS"
	LF      Token = "LF"

	// Literals
	IDENT   Token = "#IDENT"
	INTEGER Token = "#INT"
	FLOAT   Token = "#FLOAT"

	// Misc characters
	ASTERISK Token = "*"
	PERIOD   Token = "."
	COMMA    Token = ","
	EQ       Token = "="

	// Keywords
	MODULE Token = "module"
	WHERE  Token = "where"
	IMPORT Token = "import"
	IO     Token = "IO"
)
