package main

import (
	"crypto/md5"
	"fmt"
	"strconv"
	"strings"
)

type Door struct {
	Id    string
	Index int64
}

func (door *Door) String() string {
	return fmt.Sprintf("%s%d", door.Id, door.Index)
}

func (door *Door) Hash() string {
	return fmt.Sprintf("%x", md5.Sum(([]byte(door.String()))))
}

func (door *Door) NextDigit() (string, int) {
	var hash string
	for {
		door.Index++
		hash = door.Hash()
		if strings.HasPrefix(hash, "00000") {
			pos, err := strconv.Atoi(hash[5:6])
			if err == nil && pos >= 0 && pos < 8 {
				digit := hash[6:7]
				return digit, pos
			}
		}
	}
}

const passwordLength = 8

type Callback func(s string)

func (door *Door) Password(callback Callback) string {
	password := strings.Repeat("_", passwordLength)
	count := 0
	for {
		digit, pos := door.NextDigit()
		if password[pos] == '_' {
			password = replaceAtIndex(password, digit[0], pos)
			callback(password)
			count++
			if count >= passwordLength {
				break
			}
		}
	}

	return password
}

func replaceAtIndex(in string, b byte, i int) string {
	out := []byte(in)
	out[i] = b
	return string(out)
}

func main() {
	door := &Door{Id: "abbhdwsy"}
	password := door.Password(func(s string) {
		fmt.Printf("%s\r", s)
	})
	fmt.Println(password)
}
