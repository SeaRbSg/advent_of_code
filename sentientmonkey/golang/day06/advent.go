package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type Frequency struct {
	Counts []map[rune]int
}

func (frequency *Frequency) AddMessage(message string) {
	for i, r := range message {
		if len(frequency.Counts) <= i {
			frequency.Counts = append(frequency.Counts, make(map[rune]int))
		}
		frequency.Counts[i][r] += 1
	}
}

func (frequency *Frequency) AddSignal(signal string) {
	for _, message := range strings.Split(signal, "\n") {
		frequency.AddMessage(message)
	}
}

func (frequency *Frequency) ErrorCorrected() string {
	message := ""
	for _, row := range frequency.Counts {
		var maximum int
		var topLetter rune
		for letter, count := range row {
			if count >= maximum {
				topLetter = letter
				maximum = count
			}
		}
		message = message + string(topLetter)
	}

	return message
}

func main() {
	frequency := &Frequency{}
	reader := bufio.NewReader(os.Stdin)
	scanner := bufio.NewScanner(reader)
	for scanner.Scan() {
		frequency.AddSignal(scanner.Text())
	}
	fmt.Println(frequency.ErrorCorrected())
}
