package main

import "testing"
import "github.com/stretchr/testify/assert"

func TestNewSanta(t *testing.T) {
	s := Santa{}
	assert.Equal(t, 0, s.X)
	assert.Equal(t, 0, s.Y)
	assert.Equal(t, North, s.direction)
}

func TestTurnLeft(t *testing.T) {
	s := Santa{}
	s.TurnLeft()
	assert.Equal(t, West, s.direction)
}

func TestTurnRight(t *testing.T) {
	s := Santa{}
	s.TurnRight()
	assert.Equal(t, East, s.direction)
}

func TestMoveForwardNorth(t *testing.T) {
	s := Santa{}
	s.MoveForward(3)
	assert.Equal(t, 3, s.Y)
}

func TestMoveForwardEast(t *testing.T) {
	s := Santa{direction: East}
	s.MoveForward(3)
	assert.Equal(t, 3, s.X)
}

func TestMoveForwardWest(t *testing.T) {
	s := Santa{direction: West}
	s.MoveForward(3)
	assert.Equal(t, -3, s.X)
}

func TestMoveForwardSouth(t *testing.T) {
	s := NewSanta(0, 0, South)
	s.MoveForward(3)
	assert.Equal(t, -3, s.Y)
}

func TestDistance(t *testing.T) {
	s := NewSanta(2, -3, North)
	assert.Equal(t, 5, s.Distance())
}

func TestMoveL1(t *testing.T) {
	s := NewSanta(0, 0, North)
	s.Move("L1")
	assert.Equal(t, Pos{-1, 0}, s.Pos)
	assert.Equal(t, West, s.direction)
}

func TestMoveR2(t *testing.T) {
	s := NewSanta(0, 0, North)
	s.Move("R2")
	assert.Equal(t, Pos{2, 0}, s.Pos)
	assert.Equal(t, East, s.direction)
}

func TestTwoMoves(t *testing.T) {
	s := NewSanta(0, 0, North)
	s.DirectTo("R2, L3")
	assert.Equal(t, 5, s.Distance())
}

func TestThreeMoves(t *testing.T) {
	s := NewSanta(0, 0, North)
	s.DirectTo("R2, R2, R2")
	assert.Equal(t, 2, s.Distance())
}

func TestFourMoves(t *testing.T) {
	s := NewSanta(0, 0, North)
	s.DirectTo("R5, L5, R5, R3")
	assert.Equal(t, 12, s.Distance())
}

func TestVisitTwice(t *testing.T) {
	s := NewSanta(0, 0, North)
	s.DirectTo("R8, R4, R4, R8")
	pos, ok := s.GetFirstVisited()
	assert.True(t, ok)
	assert.Equal(t, Pos{4, 0}, pos)
}
