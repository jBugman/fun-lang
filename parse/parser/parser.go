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
		lit   string       // last read literal
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

func (p *Parser) scan() (tokens.Token, string) {
	if p.buf.empty {
		p.buf.tok, p.buf.lit = p.s.Scan()
	} else {
		p.buf.empty = true
	}
	return p.buf.tok, p.buf.lit
}

func (p *Parser) scanIgnoringWS() (tokens.Token, string) {
	tok, lit := p.scan()
	if tok == tokens.WS {
		tok, lit = p.scan()
	}
	return tok, lit
}

func (p *Parser) unscan() {
	p.buf.empty = false
}

func (p *Parser) syntaxErr(found string, expected tokens.Token) error {
	return fmt.Errorf("found '%s' expected '%s' at %s", found, string(expected), p.pos)
}

func (p *Parser) advancePos(tok tokens.Token) {
	p.pos.col += (p.s.N - p.pos.n) // increment by delta since last advance
	p.pos.n = p.s.N                // update from scanner
	if p.pos.lf {
		p.pos.line++
		p.pos.col = 0
		// p.pos.lf = false
	}
	p.pos.lf = tok == tokens.LF
}

// Parse parses source into fun.Module.
// Lots of copypasta while I try to get a feeling for that to refactor
func (p *Parser) Parse() (*fun.Module, error) {
	var tok tokens.Token
	var txt string
	module := &fun.Module{}

	p.skipNewlines()

	// Module declaration
	if tok, txt = p.scanIgnoringWS(); tok != tokens.MODULE {
		return nil, p.syntaxErr(txt, tokens.MODULE)
	}
	p.advancePos(tok)
	if tok, txt = p.scanIgnoringWS(); tok != tokens.IDENT {
		return nil, p.syntaxErr(txt, tokens.IDENT)
	}
	p.advancePos(tok)
	module.Name = txt
	if tok, txt = p.scanIgnoringWS(); tok != tokens.WHERE {
		return nil, p.syntaxErr(txt, tokens.WHERE)
	}
	p.advancePos(tok)
	if tok, txt = p.scanIgnoringWS(); tok != tokens.LF {
		return nil, p.syntaxErr(txt, tokens.LF)
	}
	p.advancePos(tok)

LOOP:
	for {
		p.skipNewlines()
		tok, txt = p.scanIgnoringWS()
		p.advancePos(tok)
		switch tok {
		case tokens.IMPORT:
			if tok, txt = p.scanIgnoringWS(); tok != tokens.QUOTE {
				return nil, p.syntaxErr(txt, tokens.QUOTE)
			}
			p.advancePos(tok)
			if tok, txt = p.scanIgnoringWS(); tok != tokens.IDENT {
				return nil, p.syntaxErr(txt, tokens.IDENT)
			}
			p.advancePos(tok)
			imp := fun.Import{Path: txt}
			if tok, txt = p.scanIgnoringWS(); tok != tokens.QUOTE {
				return nil, p.syntaxErr(txt, tokens.QUOTE)
			}
			p.advancePos(tok)
			tok, txt = p.scanIgnoringWS()
			p.advancePos(tok)
			switch tok {
			case tokens.AS:
				if tok, txt = p.scanIgnoringWS(); tok != tokens.QUOTE {
					return nil, p.syntaxErr(txt, tokens.QUOTE)
				}
				p.advancePos(tok)
				if tok, txt = p.scanIgnoringWS(); tok != tokens.IDENT {
					return nil, p.syntaxErr(txt, tokens.IDENT)
				}
				p.advancePos(tok)
				imp.Alias = txt
				if tok, txt = p.scanIgnoringWS(); tok != tokens.QUOTE {
					return nil, p.syntaxErr(txt, tokens.QUOTE)
				}
				p.advancePos(tok)
				if tok, txt = p.scanIgnoringWS(); tok != tokens.LF {
					return nil, p.syntaxErr(txt, tokens.LF)
				}
				p.advancePos(tok)
			case tokens.LF:
			default:
				return nil, p.syntaxErr(txt, tokens.AS)
			}
			module.Imports = append(module.Imports, imp)
		case tokens.IDENT:
			fmt.Println("found IDENT")
		case tokens.EOF:
			break LOOP // file exhausted, break parsing loop
		default:
			if !p.Debug {
				module = nil
			}
			return module, fmt.Errorf("found '%s' not expected anything at %s", txt, p.pos)
		}
	}
	return module, nil
}

func (p *Parser) skipNewlines() {
	var tok tokens.Token
LOOP:
	for {
		if tok, _ = p.scanIgnoringWS(); tok != tokens.LF {
			p.unscan()
			break LOOP
		}
		p.advancePos(tok)
	}
}
