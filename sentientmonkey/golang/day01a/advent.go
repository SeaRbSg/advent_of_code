package main

import (
	"fmt"
	"strconv"
	"strings"
)

type Direction int

const (
	North Direction = iota
	East
	South
	West
)

type Pos struct {
	X, Y int
}

type Santa struct {
	Pos
	direction     Direction
	visited       map[Pos]bool
	firstVisited  Pos
	hadFirstVisit bool
}

func NewSanta(x, y int, direction Direction) *Santa {
	return &Santa{
		Pos{x, y},
		direction,
		make(map[Pos]bool),
		Pos{},
		false,
	}
}

func abs(n int) int {
	if n < 0 {
		return -n
	} else {
		return n
	}
}

func (santa *Santa) TurnLeft() {
	n := santa.direction - 1
	if n < 0 {
		n = West
	}
	santa.direction = n
}

func (santa *Santa) TurnRight() {
	n := santa.direction + 1
	if n >= 4 {
		n = North
	}
	santa.direction = n
}

func (santa *Santa) MoveForward(n int) {
	switch santa.direction {
	case North:
		santa.Y += n
	case East:
		santa.X += n
	case South:
		santa.Y -= n
	case West:
		santa.X -= n
	}
}

func (santa *Santa) Visit() {
	if santa.visited[santa.Pos] {
		if !santa.hadFirstVisit {
			santa.firstVisited = santa.Pos
			santa.hadFirstVisit = true
		}
	} else {
		santa.visited[santa.Pos] = true
	}
}

func (santa *Santa) Move(move string) {
	switch move[0:1] {
	case "L":
		santa.TurnLeft()
	case "R":
		santa.TurnRight()
	}
	n, _ := strconv.Atoi(move[1:])
	for i := n; i > 0; i-- {
		santa.MoveForward(1)
		santa.Visit()
	}
}

func (santa *Santa) DirectTo(directions string) {

	moves := strings.Split(directions, ", ")
	for _, move := range moves {
		santa.Move(move)
	}
}

func (santa *Santa) Distance() int {
	return abs(santa.X) + abs(santa.Y)
}

func (santa *Santa) GetFirstVisited() (Pos, bool) {
	return santa.firstVisited, santa.hadFirstVisit
}

func main() {
	s := NewSanta(0, 0, North)
	s.DirectTo("L3, R2, L5, R1, L1, L2, L2, R1, R5, R1, L1, L2, R2, R4, L4, L3, L3, R5, L1, R3, L5, L2, R4, L5, R4, R2, L2, L1, R1, L3, L3, R2, R1, L4, L1, L1, R4, R5, R1, L2, L1, R188, R4, L3, R54, L4, R4, R74, R2, L4, R185, R1, R3, R5, L2, L3, R1, L1, L3, R3, R2, L3, L4, R1, L3, L5, L2, R2, L1, R2, R1, L4, R5, R4, L5, L5, L4, R5, R4, L5, L3, R4, R1, L5, L4, L3, R5, L5, L2, L4, R4, R4, R2, L1, L3, L2, R5, R4, L5, R1, R2, R5, L2, R4, R5, L2, L3, R3, L4, R3, L2, R1, R4, L5, R1, L5, L3, R4, L2, L2, L5, L5, R5, R2, L5, R1, L3, L2, L2, R3, L3, L4, R2, R3, L1, R2, L5, L3, R4, L4, R4, R3, L3, R1, L3, R5, L5, R1, R5, R3, L1")
	fmt.Printf("Day 1a: %d\n", s.Distance())
	pos, ok := s.GetFirstVisited()
	if !ok {
		fmt.Println("Day 1b: not visited")
	} else {
		fmt.Printf("Day 1b: %v\n", pos)
	}
}
