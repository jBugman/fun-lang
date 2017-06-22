package parser

import (
	"fmt"
	"io"

	"github.com/jBugman/fun-lang/fun"
	"github.com/jBugman/fun-lang/fun/tokens"
	"github.com/jBugman/fun-lang/parse/scanner"
)

// NewParser returns a new instance of Parser.
func NewParser(r io.Reader) Parser {
	p := Parser{s: scanner.NewScanner(r)}
	p.buf.empty = true
	return p
}

// Parser represents Fun parser.
type Parser struct {
	Debug bool // return result even on error
	s     *scanner.Scanner
	buf   struct {
		tok   tokens.Token // last read token
		txt   string       // last read literal
		empty bool
	}
	pos position
}

type position struct {
	line, col int  // current 'cursor' position
	n         int  // total runes read
	lf        bool // if token was LF and we should bump a line number next time
}

func (p position) String() string {
	return fmt.Sprintf("Ln %d, Col %d", p.line+1, p.col+1)
}

func (p position) adjust(txt string) position {
	return position{line: p.line, col: p.col - len(txt)}
}

func (p *Parser) scan() (tokens.Token, string) {
	if p.buf.empty {
		p.buf.tok, p.buf.txt = p.s.Scan()
	} else {
		p.buf.empty = true
	}
	p.advancePos(p.buf.tok, p.buf.txt)
	return p.buf.tok, p.buf.txt
}

func (p *Parser) scanIgnoringWS() (tokens.Token, string) {
	tok, txt := p.scan()
	if tok == tokens.WS {
		tok, txt = p.scan()
	}
	return tok, txt
}

func (p *Parser) unscan() {
	p.buf.empty = false
}

func (p *Parser) syntaxErr(found string, expected tokens.Token) error {
	return fmt.Errorf("found '%s' expected '%s' at %s", found, string(expected), p.pos.adjust(found))
}

func (p *Parser) advancePos(tok tokens.Token, txt string) {
	if p.pos.lf {
		p.pos.line++
		p.pos.col = 0 // col should be 0, given empty line + delta=1 (\n) from previous line
	}
	p.pos.col += p.s.N - p.pos.n // increment by delta since last advance
	p.pos.n = p.s.N              // update from scanner
	p.pos.lf = tok == tokens.LF
}

// Parse parses source into fun.Module.
// Lots of copypasta while I try to get a feeling for that to refactor
func (p *Parser) Parse() (*fun.Module, error) {
	var tok tokens.Token
	var txt string
	var module fun.Module
LOOP:
	for {
		p.skipNewlines()
		tok, txt = p.scanIgnoringWS()
		switch tok {
		case tokens.MODULE:
			if tok, txt = p.scanIgnoringWS(); tok != tokens.IDENT {
				return nil, p.syntaxErr(txt, tokens.IDENT)
			}
			module.Name = txt
			if tok, txt = p.scanIgnoringWS(); tok != tokens.WHERE {
				return nil, p.syntaxErr(txt, tokens.WHERE)
			}
			if tok, txt = p.scanIgnoringWS(); tok != tokens.LF {
				return nil, p.syntaxErr(txt, tokens.LF)
			}
		case tokens.IMPORT:
			if tok, txt = p.scanIgnoringWS(); tok != tokens.QUOTE {
				return nil, p.syntaxErr(txt, tokens.QUOTE)
			}
			if tok, txt = p.scanIgnoringWS(); tok != tokens.IDENT {
				return nil, p.syntaxErr(txt, tokens.IDENT)
			}
			imp := fun.Import{Path: txt}
			if tok, txt = p.scanIgnoringWS(); tok != tokens.QUOTE {
				return nil, p.syntaxErr(txt, tokens.QUOTE)
			}
			tok, txt = p.scanIgnoringWS()
			switch tok {
			case tokens.AS:
				if tok, txt = p.scanIgnoringWS(); tok != tokens.QUOTE {
					return nil, p.syntaxErr(txt, tokens.QUOTE)
				}
				if tok, txt = p.scanIgnoringWS(); tok != tokens.IDENT {
					return nil, p.syntaxErr(txt, tokens.IDENT)
				}
				imp.Alias = txt
				if tok, txt = p.scanIgnoringWS(); tok != tokens.QUOTE {
					return nil, p.syntaxErr(txt, tokens.QUOTE)
				}
				if tok, txt = p.scanIgnoringWS(); tok != tokens.LF {
					return nil, p.syntaxErr(txt, tokens.LF)
				}
			case tokens.LF:
				// no-op
			default:
				return nil, p.syntaxErr(txt, tokens.AS)
			}
			module.Imports = append(module.Imports, imp)
		case tokens.IDENT:
			decl, err := p.parseFuncDecl(tok, txt)
			if err != nil {
				return nil, err
			}
			module.Decls = append(module.Decls, *decl)
			// TODO parse other declaration types
		case tokens.EOF:
			// File exhausted, break parsing loop.
			break LOOP
		default:
			var result = &module
			if !p.Debug {
				result = nil
			}
			return result, p.syntaxErr(txt, tokens.EOF)
		}
	}
	return &module, nil
}

func (p *Parser) parseFuncDecl(tok tokens.Token, txt string) (*fun.FuncDecl, error) {
	var decl = fun.FuncDecl{Name: txt}
	if tok, txt = p.scanIgnoringWS(); tok != tokens.DOUBLECOLON {
		return nil, p.syntaxErr(txt, tokens.DOUBLECOLON)
	}
	var avaitingArrow bool
	// Type signature
TYPESIGN:
	for {
		tok, txt = p.scanIgnoringWS()
		switch tok {
		// parameter
		case tokens.IDENT:
			if avaitingArrow {
				return nil, p.syntaxErr(txt, tokens.ARROW)
			}
			t, err := p.parseType(tok, txt)
			if err != nil {
				return nil, err
			}
			var param = fun.Parameter{Type: t}
			decl.Params = append(decl.Params, param)
			avaitingArrow = true
		case tokens.ARROW:
			if !avaitingArrow {
				return nil, p.syntaxErr(txt, tokens.ARROW)
			}
			avaitingArrow = false
		// result
		case tokens.OPENBR:
			var avaitingComma bool
		TUPLE:
			for {
				tok, txt = p.scanIgnoringWS()
				switch tok {
				case tokens.IDENT:
					if avaitingComma {
						return nil, p.syntaxErr(txt, tokens.COMMA)
					}
					t, err := p.parseType(tok, txt)
					if err != nil {
						return nil, err
					}
					decl.Results.Types = append(decl.Results.Types, t)
					avaitingComma = true
				case tokens.COMMA:
					if !avaitingComma {
						return nil, p.syntaxErr(txt, tokens.IDENT)
					}
					avaitingComma = false
				case tokens.CLOSEBR:
					break TUPLE
				}
			}
		case tokens.IO:
			if avaitingArrow {
				return nil, p.syntaxErr(txt, tokens.ARROW)
			}
		case tokens.LF:
			break TYPESIGN
		}
	}
	if len(decl.Results.Types) == 0 && len(decl.Params) > 0 {
		// If there is a zero result list, then move last parameter to results
		size := len(decl.Params) - 1
		p := decl.Params[size]
		decl.Params = decl.Params[:size]
		decl.Results.Types = append(decl.Results.Types, p.Type)
	}
	// Body
	var identCount int
ARGS:
	for {
		tok, txt = p.scanIgnoringWS()
		switch tok {
		case tokens.IDENT:
			if identCount == 0 && txt != decl.Name {
				return nil, fmt.Errorf("found '%s' expected '%s' at %s", txt, decl.Name, p.pos.adjust(txt))
			}
			if identCount > 0 {
				if identCount-1 > len(decl.Params) {
					return nil, p.syntaxErr(txt, tokens.EQ)
				}
				decl.Params[identCount-1].Name = txt
			}
			identCount++
		case tokens.EQ:
			if (identCount - 1) < len(decl.Params) {
				return nil, p.syntaxErr(txt, tokens.IDENT)
			}
			break ARGS
		}
	}
	b, err := p.parseBody()
	if err != nil {
		return nil, err
	}
	decl.Body = b
	return &decl, nil
}

func (p *Parser) parseBody() (fun.FuncBody, error) {
	return fun.Undefined, nil //  TODO implement
}

func (p *Parser) parseType(tok tokens.Token, txt string) (fun.Type, error) {
	var t fun.Type
	switch txt {
	case "int":
		t = fun.IntT
	case "double":
		t = fun.DoubleT
	case "char":
		t = fun.CharT
	case "string":
		t = fun.StringT
	default:
		t = fun.ObjectType(txt)
	}
	return t, nil
}

func (p *Parser) skipNewlines() {
	var tok tokens.Token
LOOP:
	for {
		if tok, _ = p.scanIgnoringWS(); tok != tokens.LF {
			p.unscan()
			break LOOP
		}
	}
}
