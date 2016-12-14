package main

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestKeypad(t *testing.T) {
	k := NewKeypad()
	assert.NotNil(t, k)

	assert.Equal(t, 5, k.CurrentValue())

	k.MoveUp()
	assert.Equal(t, 2, k.CurrentValue())

	k.MoveDown()
	assert.Equal(t, 5, k.CurrentValue())

	k.MoveLeft()
	assert.Equal(t, 4, k.CurrentValue())

	k.MoveRight()
	assert.Equal(t, 5, k.CurrentValue())
}

func TestKeypadEdges(t *testing.T) {
	k := NewKeypad()
	k.MoveUp()
	k.MoveUp()
	assert.Equal(t, 2, k.CurrentValue())

	k = NewKeypad()
	k.MoveLeft()
	k.MoveUp()
	k.MoveUp()
	assert.Equal(t, 1, k.CurrentValue())

	k = NewKeypad()
	k.MoveLeft()
	k.MoveLeft()
	k.MoveUp()
	k.MoveUp()
	assert.Equal(t, 1, k.CurrentValue())

	k = NewKeypad()
	k.MoveUp()
	k.MoveUp()
	k.MoveLeft()
	k.MoveLeft()
	assert.Equal(t, 1, k.CurrentValue())

	k = NewKeypad()
	k.MoveRight()
	k.MoveRight()
	assert.Equal(t, 6, k.CurrentValue())

	k = NewKeypad()
	k.MoveRight()
	k.MoveRight()
	k.MoveDown()
	k.MoveDown()
	assert.Equal(t, 9, k.CurrentValue())

	k = NewKeypad()
	k.MoveDown()
	k.MoveDown()
	assert.Equal(t, 8, k.CurrentValue())
}

func TestMoves(t *testing.T) {
	k := NewKeypad()
	k.Moves("ULL")
	assert.Equal(t, 1, k.CurrentValue())

	k.Moves("RRDDD")
	assert.Equal(t, 9, k.CurrentValue())

	k.Moves("LURDL")
	assert.Equal(t, 8, k.CurrentValue())

	k.Moves("UUUUD")
	assert.Equal(t, 5, k.CurrentValue())
}

func TestGetCode(t *testing.T) {
	k := NewKeypad()
	reader := strings.NewReader("ULL\nRRDDD\nLURDL\nUUUUD")
	code := k.GetCode(reader)
	assert.Equal(t, "1985", code)
}
