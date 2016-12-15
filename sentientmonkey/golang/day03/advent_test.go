package main

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestTrianglePossible(t *testing.T) {
	triangle := Triangle{5, 10, 14}
	assert.True(t, triangle.Possible())
}

func TestTriangleImpossible(t *testing.T) {
	triangle := Triangle{5, 10, 15}
	assert.False(t, triangle.Possible())
}

func TestTriangleImpossibleAgain(t *testing.T) {
	triangle := Triangle{1, 2, 3}
	assert.False(t, triangle.Possible())
}

func TestTrianglePossibleCounts(t *testing.T) {
	input := strings.NewReader(" 5  10  14\n  5  10  15 \n 1 2 3\n")
	count := TriangleCounts(input)

	assert.Equal(t, 1, count)
}
