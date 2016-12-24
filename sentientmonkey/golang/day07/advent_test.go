package main

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestChomp(t *testing.T) {
	assert.Equal(t, "foo", chomp("foo\n"))
	assert.Equal(t, "foo", chomp("foo"))
	assert.Equal(t, "", chomp(""))
}

func TestNewIPv7(t *testing.T) {
	ip, err := NewIPv7("abba[mnop]qrst")
	assert.NoError(t, err)
	assert.Equal(t, 3, len(ip.Groups))
	assert.Equal(t, "abba", ip.Groups[0])
	assert.Equal(t, "mnop", ip.Groups[1])
	assert.Equal(t, "qrst", ip.Groups[2])
}

func TestLongIPv7(t *testing.T) {
	ip, err := NewIPv7("wysextplwqpvipxdv[srzvtwbfzqtspxnethm]syqbzgtboxxzpwr[kljvjjkjyojzrstfgrw]obdhcczonzvbfby[svotajtpttohxsh]cooktbyumlpxostt")
	assert.NoError(t, err)
	assert.Equal(t, 7, len(ip.Groups))
	assert.Equal(t, "wysextplwqpvipxdv", ip.Groups[0])
	assert.Equal(t, "srzvtwbfzqtspxnethm", ip.Groups[1])
	assert.Equal(t, "syqbzgtboxxzpwr", ip.Groups[2])
	assert.Equal(t, "kljvjjkjyojzrstfgrw", ip.Groups[3])
	assert.Equal(t, "obdhcczonzvbfby", ip.Groups[4])
	assert.Equal(t, "svotajtpttohxsh", ip.Groups[5])
	assert.Equal(t, "cooktbyumlpxostt", ip.Groups[6])
}

func TestInvalidIPv7(t *testing.T) {
	_, err := NewIPv7("abbrst")
	assert.Error(t, err)
}

func TestHasAbba(t *testing.T) {
	assert.True(t, HasAbba("abba"))
	assert.False(t, HasAbba("ab"))
	assert.True(t, HasAbba("bddb"))
	assert.True(t, HasAbba("ioxxoj"))
	assert.True(t, HasAbba("ioxxo"))
	assert.False(t, HasAbba("aaaa"))
}

func TestTLSSupported(t *testing.T) {
	var tlsTests = []struct {
		in       string
		expected bool
	}{
		{"abba[mnop]qrst", true},
		{"abcd[bddb]xyyx", false},
		{"aaaa[qwer]tyui", false},
		{"ioxxoj[asdfgh]zxcvbn", true},
		{"wysextplwqpvipxdv[srzvtwbfzqtspxnethm]syqbzgtboxxzpwr[kljvjjkjyojzrstfgrw]obdhcczonzvbfby[svotajtpttohxsh]cooktbyumlpxostt", false},
		{"otnuxdwoeqotcogo[jxyhojuczdysbfvp]upffjmvriabbalmlqmx", true},
	}

	for _, test := range tlsTests {
		ip, err := NewIPv7(test.in)
		assert.NoError(t, err)
		assert.Equal(t, test.expected, ip.TLSSupported(), fmt.Sprintf("Expected %s for %q", test.expected, test.in))
	}
}
