package main

import (
	"fmt"
	"strings"
)

type SExp interface {
	String() string
}

type Atom struct {
	value string
}

func (a Atom) String() string {
	return a.value
}

type SList struct {
	children []SExp
}

func (l SList) String() string {
	var sb strings.Builder
	sb.WriteString("(")
	for _, child := range l.children {
		sb.WriteString(child.String())
	}
	sb.WriteString(")")
	return sb.String()
}

func tokenize(input string) []string {
	input = strings.ReplaceAll(input, "(", " ( ")
	input = strings.ReplaceAll(input, ")", " ) ")
	tokens := strings.Fields(input)
	return tokens
}

func parseTokens(tokens []string) (SExp, []string, error) {
	if len(tokens) == 0 {
		return nil, tokens, fmt.Errorf("unexpected end of input")
	}

	token := tokens[0]
	tokens = tokens[1:]
	if token == "(" {
		children := []SExp{}
		for len(tokens) > 0 && tokens[0] != ")" {
			child, rest, err := parseTokens(tokens)
			if err != nil {
				return nil, tokens, err
			}
			children = append(children, child)
			tokens = rest
		}
		if len(tokens) == 0 {
			return nil, tokens, fmt.Errorf("unmatched (")
		}
		tokens = tokens[1:] // Remove ")"
		return SList{children: children}, tokens, nil
	} else if token == ")" {
		return nil, tokens, fmt.Errorf("unmatched )")
	} else {
		return Atom{value: token}, tokens, nil
	}
}

func parse(input string) (SExp, error) {
	tokens := tokenize(input)
	sexp, rest, err := parseTokens(tokens)
	if err != nil {
		return nil, err
	}
	if len(rest) != 0 {
		return nil, fmt.Errorf("invalid s-expression")
	}
	return sexp, nil
}

func main() {
	input := "(+ 1 (* 2 3))"
	result, err := parse(input)
	if err != nil {
		fmt.Println("Error:", err)
		return
	}
	fmt.Println(result)
}
