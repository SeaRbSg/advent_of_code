package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"regexp"
	"sort"
	"strconv"
	"strings"
)

type Room struct {
	Name     string
	Sector   int
	Checksum string
}

var format = regexp.MustCompile(`([a-z0-9\-]+)\[([a-z]+)\]`)

func NewRoom(s string) (*Room, error) {
	matches := format.FindStringSubmatch(s)

	if len(matches) != 3 {
		return nil, fmt.Errorf("Could not parse %q", s)
	}
	groups := strings.Split(matches[1], "-")
	sector, err := strconv.Atoi(groups[len(groups)-1])
	if err != nil {
		return nil, err
	}

	return &Room{Name: strings.Join(groups[0:len(groups)-1], "-"),
		Sector:   sector,
		Checksum: matches[2],
	}, nil
}

type checksum struct {
	char  string
	count int
}

type checksumList []checksum

func (c checksumList) Len() int {
	return len(c)
}

func (c checksumList) Swap(i, j int) {
	c[i], c[j] = c[j], c[i]
}

func (c checksumList) Less(i, j int) bool {
	if c[i].count == c[j].count {
		return c[i].char < c[j].char
	}

	return c[i].count > c[j].count
}

func (c checksumList) String() string {
	result := ""
	for i, key := range c {
		if i >= 5 {
			break
		}
		result += key.char
	}

	return result
}

func (room *Room) frequency() map[string]int {
	counts := make(map[string]int)
	for _, chars := range strings.Split(room.Name, "-") {
		for _, c := range chars {
			s := string(c)
			if s >= "a" && s <= "z" {
				counts[s] += 1
			}
		}
	}

	return counts
}

func (room *Room) calculateChecksum() string {
	var keys checksumList
	for k, v := range room.frequency() {
		keys = append(keys, checksum{k, v})
	}
	sort.Sort(keys)

	return keys.String()
}

func (room *Room) IsValid() bool {
	return room.calculateChecksum() == room.Checksum
}

func SectorSum(reader io.Reader) int {
	count := 0
	scanner := bufio.NewScanner(reader)
	for scanner.Scan() {
		var s string
		_, err := fmt.Sscanf(scanner.Text(), "%s\n", &s)
		if err != nil {
			continue
		}
		room, err := NewRoom(s)
		if err != nil {
			continue
		}
		if room.IsValid() {
			count += room.Sector
		}
	}
	return count
}

func main() {
	fmt.Println(SectorSum(os.Stdin))
}
