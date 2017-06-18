package tokens

// Token represents valid Fun token
type Token uint8

// Possible Token values
const (
	// Special tokens
	ILLEGAL Token = iota
	EOF
	WS
	LF
	// Literals
	IDENT // identifier
	INTEGER
	FLOAT
	// Misc characters
	ASTERISK // *
	PERIOD   // .
	COMMA    // ,
	EQ       // =
	// Keywords
	MODULE
	WHERE
	IMPORT
	IO
)

func (t Token) String() string {
	switch t {
	// Special
	case ILLEGAL:
		return "#illegal"
	case EOF:
		return "#eof"
	case WS:
		return " "
	case LF:
		return "\n"
	// Literals
	case IDENT:
		return "#ident"
	case INTEGER:
		return "#int"
	case FLOAT:
		return "#float"
	// Characters
	case ASTERISK:
		return "*"
	case PERIOD:
		return "."
	case COMMA:
		return ","
	case EQ:
		return "="
		// Keywords
	case MODULE:
		return "module"
	case WHERE:
		return "where"
	case IMPORT:
		return "import"
	case IO:
		return "IO"
	default:
		return "#undefined"
	}
}
