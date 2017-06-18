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

	// Language constructs
	ARROW       Token = "->" // TODO add
	DOUBLECOLON Token = "::" // TODO add
	UNIT        Token = "()" // TODO add

	// Misc characters
	COLON    Token = ":"
	ASTERISK Token = "*"
	PERIOD   Token = "."
	COMMA    Token = ","
	EQ       Token = "="
	OPENBR   Token = "("
	CLOSEBR  Token = ")"

	// Keywords
	MODULE    Token = "module"
	WHERE     Token = "where"
	IMPORT    Token = "import"
	IO        Token = "IO"
	UNDEFINED Token = "undefined"
)
