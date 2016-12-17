package main

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestRoom(t *testing.T) {
	r, err := NewRoom("aaaaa-bbb-z-y-x-123[abxyz]")
	assert.NoError(t, err)
	assert.Equal(t, "aaaaa-bbb-z-y-x", r.Name)
	assert.Equal(t, "abxyz", r.Checksum)
	assert.Equal(t, 123, r.Sector)
}

func TestValidFormatRoom(t *testing.T) {
	r, err := NewRoom("tacos")
	assert.Error(t, err)
	assert.Nil(t, r)
}

func TestValidRoom(t *testing.T) {
	r, _ := NewRoom("aaaaa-bbb-z-y-x-123[abxyz]")
	assert.Equal(t, "abxyz", r.Checksum)
	assert.Equal(t, "abxyz", r.calculateChecksum())
	assert.True(t, r.IsValid())
}

func TestValidRoomByLetters(t *testing.T) {
	r, _ := NewRoom("a-b-c-d-e-f-g-h-987[abcde]")
	assert.True(t, r.IsValid())
}

func TestValidRoomByCounts(t *testing.T) {
	r, _ := NewRoom("not-a-real-room-404[oarel]")
	assert.True(t, r.IsValid())
}

func TestValidRoomDecoy(t *testing.T) {
	r, _ := NewRoom("totally-real-room-200[decoy]")
	assert.False(t, r.IsValid())
}

var testRooms = `aaaaa-bbb-z-y-x-123[abxyz]
a-b-c-d-e-f-g-h-987[abcde]
not-a-real-room-404[oarel]
totally-real-room-200[decoy]
`

func TestSectorCounts(t *testing.T) {
	r := strings.NewReader(testRooms)
	assert.Equal(t, 1514, SectorSum(r))
}
