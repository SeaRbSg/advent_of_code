package main

import (
	"bufio"
	"flag"
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
		if x, y, z, err := readLine(scanner); err == nil {
			triangle := Triangle{x, y, z}
			if triangle.Possible() {
				count++
			}
		}
	}

	return count
}

func TriangleVerticalCounts(reader io.Reader) int {
	count := 0
	scanner := bufio.NewScanner(reader)
	i := 0
	var a, b, c [3]int
	for scanner.Scan() {
		if x, y, z, err := readLine(scanner); err == nil {
			a[i%3], b[i%3], c[i%3] = x, y, z
			if i%3 == 2 {
				points := [][3]int{a, b, c}
				for _, p := range points {
					triangle := Triangle{p[0], p[1], p[2]}
					if triangle.Possible() {
						count++
					}
				}
			}
		}
		i++
	}

	return count
}

func readLine(scanner *bufio.Scanner) (int, int, int, error) {
	var x, y, z int
	_, err := fmt.Sscanf(scanner.Text(), "%d %d %d\n", &x, &y, &z)
	return x, y, z, err
}

func main() {
	var vertical bool
	var counts int

	flag.BoolVar(&vertical, "vertical", false, "read vertical")
	flag.Parse()

	if vertical {
		counts = TriangleVerticalCounts(os.Stdin)
	} else {
		counts = TriangleCounts(os.Stdin)
	}
	fmt.Println(counts)
}
