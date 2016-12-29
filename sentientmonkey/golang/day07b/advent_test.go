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

func TestHasAba(t *testing.T) {
	var abaTests = []struct {
		in   string
		abas []string
	}{
		{"aba", []string{"aba"}},
		{"ab", []string{}},
		{"bdb", []string{"bdb"}},
		{"ioxoj", []string{"oxo"}},
		{"ioxo", []string{"oxo"}},
		{"aaa", []string{}},
		{"zazbz", []string{"zaz", "zbz"}},
	}

	for _, abaTest := range abaTests {
		abas := GetAbas(abaTest.in)
		assert.Equal(t, abaTest.abas, abas)
	}
}

func TestBab(t *testing.T) {
	var babTests = []struct {
		in     string
		bab    string
		hasBab bool
	}{
		{"aba", "bab", true},
		{"ab", "", false},
		{"bdb", "dbd", true},
		{"oxo", "xox", true},
		{"aaa", "", false},
	}

	for _, babTest := range babTests {
		bab, ok := GetBab(babTest.in)
		assert.Equal(t, babTest.hasBab, ok)
		assert.Equal(t, babTest.bab, bab)
	}

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

func TestSSLSupported(t *testing.T) {
	var sslTests = []struct {
		in       string
		expected bool
	}{
		{"aba[bab]xyz", true},
		{"xyx[xyx]xyx", false},
		{"aaa[kek]eke", true},
		{"zazbz[bzb]cdb", true},
	}

	for _, test := range sslTests {
		ip, err := NewIPv7(test.in)
		assert.NoError(t, err)
		assert.Equal(t, test.expected, ip.SSLSupported(), fmt.Sprintf("Expected %t for %q", test.expected, test.in))
	}

}
