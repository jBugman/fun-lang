package tokens

// Token represents valid Fun token
type Token string

// Possible Token values
const (
	// Special tokens
	ILLEGAL Token = "ILLEGAL"
	EOF     Token = "EOF"
	WS      Token = "WS"
	LF      Token = "LF"

	// Literals
	IDENT   Token = "IDENT"
	INTEGER Token = "INT"
	FLOAT   Token = "FLOAT"

	// Language constructs
	ARROW       Token = "->"
	DOUBLECOLON Token = "::"
	UNIT        Token = "()"

	// Misc characters
	QUOTE    Token = "\""
	COLON    Token = ":"
	ASTERISK Token = "*"
	PLUS     Token = "+"
	DASH     Token = "-"
	SLASH    Token = "/"
	PERCENT  Token = "%"
	PERIOD   Token = "."
	COMMA    Token = ","
	EQ       Token = "="
	OPENBR   Token = "("
	CLOSEBR  Token = ")"

	// Keywords
	MODULE    Token = "module"
	WHERE     Token = "where"
	IMPORT    Token = "import"
	AS        Token = "as"
	IO        Token = "IO"
	UNDEFINED Token = "undefined"
	IF        Token = "if"
	THEN      Token = "then"
	ELSE      Token = "else"
	TYPE      Token = "type"

	// Go-reserved keywords
	RESERVED  Token = "RESERVED" // catch-all token just in case
	BREAK     Token = "break"
	DEFAULT   Token = "default"
	FUNC      Token = "func"
	INTERFACE Token = "interface"
	SELECT    Token = "select"
	PACKAGE   Token = "package"
	CONST     Token = "const"
	FOR       Token = "for"
	RETURN    Token = "return"
	VAR       Token = "var"
	// CASE
	// DEFER
	// GO
	// MAP
	// STRUCT
	// CHAN
	// GOTO
	// SWITCH
	// FALLTHROUGH
	// RANGE
	// CONTINUE
)
