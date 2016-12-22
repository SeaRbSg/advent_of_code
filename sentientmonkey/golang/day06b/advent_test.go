package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestAddMessage(t *testing.T) {
	frequency := &Frequency{}
	frequency.AddMessage("eedadn")
	var testResults = []map[rune]int{
		{'e': 1},
		{'e': 1},
		{'d': 1},
		{'a': 1},
		{'d': 1},
		{'n': 1},
	}
	assert.Equal(t, testResults, frequency.Counts)
}

var testSignal = `eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar`

func TestAddSignal(t *testing.T) {
	frequency := &Frequency{}
	frequency.AddSignal(testSignal)
	assert.Equal(t, "advent", frequency.ErrorCorrected())
}
