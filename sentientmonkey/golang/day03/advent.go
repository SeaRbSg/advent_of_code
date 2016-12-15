package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"sort"
)

type Triangle struct {
	x, y, z int
}

func (t Triangle) Possible() bool {
	ints := []int{t.x, t.y, t.z}
	sort.Ints(ints)
	return ints[0]+ints[1] > ints[2]
}

func TriangleCounts(reader io.Reader) int {
	count := 0
	scanner := bufio.NewScanner(reader)
	for scanner.Scan() {
		var x, y, z int
		_, err := fmt.Sscanf(scanner.Text(), "%d %d %d\n", &x, &y, &z)
		if err == nil {
			triangle := Triangle{x, y, z}
			if triangle.Possible() {
				count++
			}
		}
	}

	return count
}

func main() {
	counts := TriangleCounts(os.Stdin)
	fmt.Println(counts)
}
