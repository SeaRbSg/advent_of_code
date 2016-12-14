package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
)

type Keypad struct {
	value int
}

func NewKeypad() *Keypad {
	return &Keypad{5}
}

func (keypad *Keypad) CurrentValue() int {
	return keypad.value
}

func (keypad *Keypad) MoveUp() {
	if keypad.value > 3 {
		keypad.value -= 3
	}
}

func (keypad *Keypad) MoveDown() {
	if keypad.value < 7 {
		keypad.value += 3
	}
}

func (keypad *Keypad) MoveLeft() {
	if keypad.value%3 != 1 {
		keypad.value -= 1
	}
}

func (keypad *Keypad) MoveRight() {
	if keypad.value%3 != 0 {
		keypad.value += 1
	}
}

func (keypad *Keypad) Moves(s string) {
	for _, m := range s {
		switch m {
		case 'U':
			keypad.MoveUp()
		case 'D':
			keypad.MoveDown()
		case 'L':
			keypad.MoveLeft()
		case 'R':
			keypad.MoveRight()
		}
	}
}

func (keypad *Keypad) GetCode(reader io.Reader) string {
	code := ""
	scanner := bufio.NewScanner(reader)
	for scanner.Scan() {
		keypad.Moves(scanner.Text())
		digit := fmt.Sprintf("%d", keypad.CurrentValue())
		code += digit
	}

	return code
}

func main() {
	keypad := NewKeypad()
	code := keypad.GetCode(os.Stdin)
	fmt.Println(code)
}
