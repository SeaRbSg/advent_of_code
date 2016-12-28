package main

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestCopy(t *testing.T) {
	cpu := NewCpu()
	cpu.Copy(41, "a")
	assert.Equal(t, 41, cpu.registers["a"])
}

func TestIncrease(t *testing.T) {
	cpu := NewCpu()
	cpu.Copy(41, "a")
	cpu.Increase("a")
	assert.Equal(t, 42, cpu.registers["a"])
}

func TestDecrease(t *testing.T) {
	cpu := NewCpu()
	cpu.Copy(41, "a")
	cpu.Decrease("a")
	assert.Equal(t, 40, cpu.registers["a"])
}

func TestRun(t *testing.T) {
	cpu := NewCpu()

	instructions := []Instruction{
		{"cpy", "41", "a"},
		{"inc", "a", ""},
		{"cpy", "41", "b"},
		{"dec", "b", ""},
	}

	cpu.Run(instructions)
	assert.Equal(t, 42, cpu.registers["a"])
	assert.Equal(t, 40, cpu.registers["b"])
}

func TestJumpIf(t *testing.T) {
	cpu := NewCpu()

	instructions := []Instruction{
		{"cpy", "41", "a"},
		{"inc", "a", ""},
		{"inc", "a", ""},
		{"dec", "a", ""},
		{"jnz", "a", "2"},
		{"dec", "a", ""},
	}

	cpu.Run(instructions)
	assert.Equal(t, 42, cpu.registers["a"])
}

func TestCopyVariable(t *testing.T) {
	cpu := NewCpu()

	instructions := []Instruction{
		{"cpy", "41", "a"},
		{"cpy", "a", "b"},
	}

	cpu.Run(instructions)
	assert.Equal(t, 41, cpu.registers["b"])
}

func TestJumpIfInteger(t *testing.T) {
	cpu := NewCpu()

	instructions := []Instruction{
		{"cpy", "41", "a"},
		{"jnz", "1", "2"},
		{"inc", "a", ""},
	}

	cpu.Run(instructions)
	assert.Equal(t, 41, cpu.registers["a"])
}

func TestParseProgram(t *testing.T) {
	cpu := NewCpu()

	program := `cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a`

	instructions := cpu.Parse(strings.NewReader(program))
	expected := []Instruction{
		{"cpy", "41", "a"},
		{"inc", "a", ""},
		{"inc", "a", ""},
		{"dec", "a", ""},
		{"jnz", "a", "2"},
		{"dec", "a", ""},
	}

	assert.Equal(t, expected, instructions)
}

func TestExecuteProgram(t *testing.T) {
	cpu := NewCpu()

	program := `cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a`

	cpu.Execute(strings.NewReader(program))

	assert.Equal(t, 42, cpu.registers["a"])
}
