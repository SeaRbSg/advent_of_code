package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type IPv7 struct {
	Groups []string
}

func NewIPv7(address string) (*IPv7, error) {

	matches := strings.FieldsFunc(address, func(r rune) bool {
		return r == '[' || r == ']'
	})

	if len(matches) < 3 {
		return nil, fmt.Errorf("Not a valid IPv7 address: %q", address)
	}

	return &IPv7{matches}, nil
}

func HasAbba(group string) bool {
	for i := 0; i <= len(group)-4; i++ {
		a, b, c, d := group[i], group[i+1], group[i+2], group[i+3]
		if a == d && b == c && a != b {
			return true
		}
	}

	return false
}

func (ip *IPv7) TLSSupported() bool {
	foundAbba := false
	for i, group := range ip.Groups {
		// odd, in brackets
		if i%2 == 1 {
			if HasAbba(group) {
				return false
			}
			// even, outside brackets
		} else {
			if HasAbba(group) {
				foundAbba = true
			}
		}
	}

	return foundAbba
}

func chomp(s string) string {
	if len(s) > 0 && s[len(s)-1:] == "\n" {
		return s[:len(s)-1]
	}
	return s
}

func main() {
	reader := bufio.NewReader(os.Stdin)
	scanner := bufio.NewScanner(reader)
	count := 0
	for scanner.Scan() {
		address := scanner.Text()
		address = chomp(address)
		ipv7, err := NewIPv7(address)
		if err != nil {
			panic(err)
		}
		if ipv7.TLSSupported() {
			count++
		}
	}
	fmt.Printf("%d\n", count)
}
