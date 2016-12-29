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

func GetAbas(group string) []string {
	abas := []string{}
	for i := 0; i <= len(group)-3; i++ {
		a, b, c := group[i], group[i+1], group[i+2]
		if a == c && a != b {
			abas = append(abas, group[i:i+3])
		}
	}

	return abas
}

func GetBab(aba string) (string, bool) {
	if len(aba) != 3 {
		return "", false
	}
	a, b, c := aba[0:1], aba[1:2], aba[2:3]
	if a != c || a == b {
		return "", false
	}

	return b + a + b, true
}

func (ip *IPv7) SSLSupported() bool {
	hypernets := make(map[string]bool)
	supernets := make(map[string]bool)
	for i, group := range ip.Groups {
		// odd, in hypernet
		if i%2 == 1 {
			for _, aba := range GetAbas(group) {
				hypernets[aba] = true
			}
			// even, in supernet
		} else {
			for _, aba := range GetAbas(group) {
				supernets[aba] = true
			}
		}
	}

	if len(hypernets) == 0 || len(supernets) == 0 {
		return false
	}

	for aba := range supernets {
		bab, _ := GetBab(aba)
		if _, ok := hypernets[bab]; ok {
			return true
		}
	}

	return false
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
