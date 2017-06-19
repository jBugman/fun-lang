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
	s   *scanner.Scanner
	buf struct {
		tok   tokens.Token // last read token
		lit   string       // last read literal
		empty bool
	}
	pos struct {
		line, col uint // current 'cursor' position
	}
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

// Parse parses source into fun.Module.
func (p *Parser) Parse() (*fun.Module, error) {
	var tok tokens.Token
	var txt string

	// placeholder loop to test if it works at all
	for tok != tokens.EOF {
		tok, txt = p.s.Scan()
		fmt.Println(p.pos, txt)

		// Advance position
		p.pos.col++
		if tok == tokens.LF {
			p.pos.line++
			p.pos.col = 0
		}
	}
	return nil, nil
}
