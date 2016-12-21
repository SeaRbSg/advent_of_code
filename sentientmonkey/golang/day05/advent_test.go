package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestDoor(t *testing.T) {
	door := &Door{Id: "abc", Index: 3231929}
	assert.Equal(t, "abc3231929", door.String())
	assert.Equal(t, "00000155f8105dff7f56ee10fa9b9abd", door.Hash())
}

func TestDoorFindDigit(t *testing.T) {
	door := &Door{Id: "abc"}
	assert.Equal(t, "1", door.NextDigit())
	assert.Equal(t, "8", door.NextDigit())
}

func TestDoorPassword(t *testing.T) {
	door := &Door{Id: "abc"}
	assert.Equal(t, "18f47a30", door.Password())
}
