package main

import (
	"crypto/md5"
	"fmt"
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

func (door *Door) NextDigit() string {
	var hash string
	for {
		door.Index++
		hash = door.Hash()
		if strings.HasPrefix(hash, "00000") {
			break
		}
	}

	return hash[5:6]
}

func (door *Door) Password() string {
	var password string
	for i := 0; i < 8; i++ {
		password += door.NextDigit()
	}
	return password
}

func main() {
	door := &Door{Id: "abbhdwsy"}
	fmt.Println(door.Password())
}
