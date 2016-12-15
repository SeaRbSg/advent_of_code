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

func TestTriangleImpossibleUnordered(t *testing.T) {
	triangle := Triangle{10, 4, 25}
	assert.False(t, triangle.Possible())
}

func TestTriangleImpossibleAgain(t *testing.T) {
	triangle := Triangle{1, 2, 3}
	assert.False(t, triangle.Possible())
}

func TestTrianglePossibleCounts(t *testing.T) {
	testCounts := `5  10  14
 5  10  15
 1   2   3
`

	input := strings.NewReader(testCounts)
	count := TriangleCounts(input)

	assert.Equal(t, 1, count)
}

func TestTriangleVerticalPossibleCounts(t *testing.T) {
	testVerticalCounts := `101 301 501
102 302 502
103 303 503
201 401 601
202 402 602
203 403 603
`
	input := strings.NewReader(testVerticalCounts)
	count := TriangleVerticalCounts(input)

	assert.Equal(t, 6, count)
}
