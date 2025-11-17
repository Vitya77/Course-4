package main

import (
	"bufio"
	"errors"
	"fmt"
	"os"
	"strconv"
	"strings"
	"unicode"
)

type ParseError struct {
	Msg string
	Pos int
}

func (e ParseError) Error() string {
	if e.Pos >= 0 {
		return fmt.Sprintf("ParseError at pos %d: %s", e.Pos, e.Msg)
	}
	return "ParseError: " + e.Msg
}

type EvalError struct {
	Msg string
}

func (e EvalError) Error() string { return "EvalError: " + e.Msg }

type DivideByZeroError struct {
	Msg string
}

func (e DivideByZeroError) Error() string { return "DivideByZeroError: " + e.Msg }

type TreeError struct {
	Msg string
}

func (e TreeError) Error() string { return "TreeError: " + e.Msg }

type TokenKind int

const (
	TokNumber TokenKind = iota
	TokOperator
	TokLParen
	TokRParen
)

type Token struct {
	Kind  TokenKind
	Value string
	Pos   int
}

func tokenize(input string) ([]Token, error) {
	var toks []Token
	s := strings.TrimSpace(input)
	i := 0
	for i < len(s) {
		ch := s[i]
		if unicode.IsSpace(rune(ch)) {
			i++
			continue
		}

		if ch == '(' {
			toks = append(toks, Token{Kind: TokLParen, Value: "(", Pos: i})
			i++
			continue
		}
		if ch == ')' {
			toks = append(toks, Token{Kind: TokRParen, Value: ")", Pos: i})
			i++
			continue
		}

		if strings.ContainsRune("+-*/", rune(ch)) {
			isUnary := false
			if ch == '-' {
				if len(toks) == 0 {
					isUnary = true
				} else {
					prev := toks[len(toks)-1]
					if prev.Kind == TokOperator || prev.Kind == TokLParen {
						isUnary = true
					}
				}
			}
			if ch == '-' && isUnary {
				toks = append(toks, Token{Kind: TokOperator, Value: "neg", Pos: i})
			} else {
				toks = append(toks, Token{Kind: TokOperator, Value: string(ch), Pos: i})
			}
			i++
			continue
		}

		if unicode.IsDigit(rune(ch)) || ch == '.' {
			start := i
			dotCount := 0
			for i < len(s) && (unicode.IsDigit(rune(s[i])) || s[i] == '.') {
				if s[i] == '.' {
					dotCount++
					if dotCount > 1 {
						return nil, ParseError{Msg: "invalid numeric literal (multiple dots)", Pos: start}
					}
				}
				i++
			}
			numStr := s[start:i]

			if numStr == "." {
				return nil, ParseError{Msg: "invalid numeric literal '.'", Pos: start}
			}
			toks = append(toks, Token{Kind: TokNumber, Value: numStr, Pos: start})
			continue
		}

		return nil, ParseError{Msg: fmt.Sprintf("unexpected character '%c'", ch), Pos: i}
	}
	return toks, nil
}

type Assoc int

const (
	Left Assoc = iota
	Right
)

var opPrecedence = map[string]int{
	"+":   1,
	"-":   1,
	"*":   2,
	"/":   2,
	"neg": 3,
}

var opAssoc = map[string]Assoc{
	"+":   Left,
	"-":   Left,
	"*":   Left,
	"/":   Left,
	"neg": Right,
}

func shuntingYard(tokens []Token) ([]Token, error) {
	var output []Token
	var stack []Token

	for idx := 0; idx < len(tokens); idx++ {
		t := tokens[idx]
		switch t.Kind {
		case TokNumber:
			output = append(output, t)
		case TokOperator:
			for len(stack) > 0 {
				top := stack[len(stack)-1]
				if top.Kind != TokOperator {
					break
				}
				precTop, okTop := opPrecedence[top.Value]
				precT, okT := opPrecedence[t.Value]
				if !okTop || !okT {
					return nil, ParseError{Msg: "unknown operator", Pos: t.Pos}
				}
				if (opAssoc[t.Value] == Left && precT <= precTop) || (opAssoc[t.Value] == Right && precT < precTop) {
					output = append(output, top)
					stack = stack[:len(stack)-1]
					continue
				}
				break
			}
			stack = append(stack, t)
		case TokLParen:
			stack = append(stack, t)
		case TokRParen:
			found := false
			for len(stack) > 0 {
				top := stack[len(stack)-1]
				stack = stack[:len(stack)-1]
				if top.Kind == TokLParen {
					found = true
					break
				}
				output = append(output, top)
			}
			if !found {
				return nil, ParseError{Msg: "mismatched parentheses", Pos: t.Pos}
			}
		default:
			return nil, ParseError{Msg: "unexpected token during shunting", Pos: t.Pos}
		}
	}

	for len(stack) > 0 {
		top := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		if top.Kind == TokLParen || top.Kind == TokRParen {
			return nil, ParseError{Msg: "mismatched parentheses", Pos: top.Pos}
		}
		output = append(output, top)
	}
	return output, nil
}

type Node interface {
	Eval() (float64, error)
	Prefix() string
	Postfix() string
	Pretty(indent string, last bool) []string
}

type NumberNode struct {
	Value float64
}

func (n *NumberNode) Eval() (float64, error) {
	return n.Value, nil
}
func (n *NumberNode) Prefix() string {
	return fmt.Sprintf("%v", n.Value)
}
func (n *NumberNode) Postfix() string {
	return fmt.Sprintf("%v", n.Value)
}
func (n *NumberNode) Pretty(indent string, last bool) []string {
	head := fmt.Sprintf("%v", n.Value)
	return []string{indent + head}
}

type UnaryNode struct {
	Op    string
	Child Node
}

func (u *UnaryNode) Eval() (float64, error) {
	defer func() {
		if r := recover(); r != nil {
			panic(r)
		}
	}()
	val, err := u.Child.Eval()
	if err != nil {
		return 0, err
	}
	switch u.Op {
	case "neg":
		return -val, nil
	default:
		return 0, EvalError{Msg: "unknown unary operator " + u.Op}
	}
}
func (u *UnaryNode) Prefix() string {
	return fmt.Sprintf("(%s %s)", u.Op, u.Child.Prefix())
}
func (u *UnaryNode) Postfix() string {
	return fmt.Sprintf("(%s %s)", u.Child.Postfix(), u.Op)
}
func (u *UnaryNode) Pretty(indent string, last bool) []string {
	var lines []string
	lines = append(lines, indent+u.Op)
	childIndent := indent + "  "
	childLines := u.Child.Pretty(childIndent, true)
	lines = append(lines, childLines...)
	return lines
}

type BinaryNode struct {
	Op    string
	Left  Node
	Right Node
}

func (b *BinaryNode) Eval() (float64, error) {
	defer func() {
		if r := recover(); r != nil {
			panic(r)
		}
	}()
	lv, err := b.Left.Eval()
	if err != nil {
		return 0, err
	}
	rv, err := b.Right.Eval()
	if err != nil {
		return 0, err
	}
	switch b.Op {
	case "+":
		return lv + rv, nil
	case "-":
		return lv - rv, nil
	case "*":
		return lv * rv, nil
	case "/":
		if rv == 0 {
			return 0, DivideByZeroError{Msg: "division by zero"}
		}
		return lv / rv, nil
	default:
		return 0, EvalError{Msg: "unknown binary operator " + b.Op}
	}
}
func (b *BinaryNode) Prefix() string {
	return fmt.Sprintf("(%s %s %s)", b.Op, b.Left.Prefix(), b.Right.Prefix())
}
func (b *BinaryNode) Postfix() string {
	return fmt.Sprintf("(%s %s %s)", b.Left.Postfix(), b.Right.Postfix(), b.Op)
}
func (b *BinaryNode) Pretty(indent string, last bool) []string {
	var lines []string
	lines = append(lines, indent+b.Op)
	childIndent := indent + "  "
	leftLines := b.Left.Pretty(childIndent, false)
	rightLines := b.Right.Pretty(childIndent, true)
	lines = append(lines, leftLines...)
	lines = append(lines, rightLines...)
	return lines
}

func buildASTFromRPN(rpn []Token) (Node, error) {
	var stack []Node
	for _, t := range rpn {
		switch t.Kind {
		case TokNumber:
			f, err := strconv.ParseFloat(t.Value, 64)
			if err != nil {
				return nil, ParseError{Msg: "invalid number: " + t.Value, Pos: t.Pos}
			}
			stack = append(stack, &NumberNode{Value: f})
		case TokOperator:
			if t.Value == "neg" {
				if len(stack) < 1 {
					return nil, TreeError{Msg: "not enough operands for unary operator"}
				}
				child := stack[len(stack)-1]
				stack = stack[:len(stack)-1]
				stack = append(stack, &UnaryNode{Op: "neg", Child: child})
			} else {
				if len(stack) < 2 {
					return nil, TreeError{Msg: "not enough operands for binary operator " + t.Value}
				}
				right := stack[len(stack)-1]
				left := stack[len(stack)-2]
				stack = stack[:len(stack)-2]
				stack = append(stack, &BinaryNode{Op: t.Value, Left: left, Right: right})
			}
		default:
			return nil, TreeError{Msg: "unexpected token in RPN"}
		}
	}
	if len(stack) != 1 {
		return nil, TreeError{Msg: "invalid expression: leftover nodes in stack"}
	}
	return stack[0], nil
}

func printTree(root Node) {
	fmt.Println("Форматоване дерево (top-down):")
	lines := root.Pretty("", true)
	for _, l := range lines {
		fmt.Println(l)
	}
}

func processExpression(input string) error {
	defer func() {
		if r := recover(); r != nil {
			fmt.Fprintf(os.Stderr, "panic intercepted: %v\n", r)
		}
	}()

	toks, err := tokenize(input)
	if err != nil {
		return err
	}

	rpn, err := shuntingYard(toks)
	if err != nil {
		return err
	}

	var rpnStrs []string
	for _, t := range rpn {
		rpnStrs = append(rpnStrs, t.Value)
	}
	fmt.Println("Постфіксний (RPN):", strings.Join(rpnStrs, " "))

	root, err := buildASTFromRPN(rpn)
	if err != nil {
		return err
	}

	printTree(root)

	fmt.Println("Префіксний запис:", root.Prefix())
	fmt.Println("Постфіксний запис:", root.Postfix())

	val, err := root.Eval()
	if err != nil {
		return err
	}
	fmt.Printf("Значення виразу = %v\n", val)
	return nil
}

func main() {
	fmt.Println("Введіть арифметичний вираз (підтримується + - * /, дужки, унарний '-').")
	fmt.Print("> ")
	reader := bufio.NewReader(os.Stdin)
	line, err := reader.ReadString('\n')
	if err != nil {
		if errors.Is(err, os.ErrClosed) || err.Error() == "EOF" {
			fmt.Println("Вихід.")
			return
		}
		fmt.Fprintln(os.Stderr, "read error:", err)
		return
	}
	line = strings.TrimSpace(line)
	if line == "" {
		fmt.Println("Порожній рядок - вихід.")
		return
	}

	defer func() {
		if r := recover(); r != nil {
			fmt.Fprintln(os.Stderr, "Критична помилка (panic):", r)
		}
	}()

	err = processExpression(line)
	if err != nil {
		fmt.Fprintln(os.Stderr, "Помилка:", err)
		return
	}
}
