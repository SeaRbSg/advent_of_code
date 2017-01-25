package main

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParseVariable(t *testing.T) {
	cpu := NewCpu()
	a := cpu.ParseVariable("a")
	one := cpu.ParseVariable("1")

	assert.Equal(t, "a", a)
	assert.Equal(t, 1, one)
}

func TestSetGetRegister(t *testing.T) {
	cpu := NewCpu()

	cpu.SetRegister("a", 42)
	value := cpu.GetRegister("a")
	assert.Equal(t, 42, value)

	value = cpu.GetRegister("b")
	assert.Zero(t, value)
}

func TestCopy(t *testing.T) {
	cpu := NewCpu()
	cpu.Copy(41, "a")
	assert.Equal(t, 41, cpu.GetRegister("a"))
}

func TestIncrease(t *testing.T) {
	cpu := NewCpu()
	cpu.Copy(41, "a")
	cpu.Increase("a")
	assert.Equal(t, 42, cpu.GetRegister("a"))
}

func TestDecrease(t *testing.T) {
	cpu := NewCpu()
	cpu.Copy(41, "a")
	cpu.Decrease("a")
	assert.Equal(t, 40, cpu.GetRegister("a"))
}

func TestRun(t *testing.T) {
	cpu := NewCpu()

	cpu.instructions = []Instruction{
		{cpy, 41, "a"},
		{inc, "a", nil},
		{cpy, 41, "b"},
		{dec, "b", nil},
	}

	cpu.Run()
	assert.Equal(t, 42, cpu.GetRegister("a"))
	assert.Equal(t, 40, cpu.GetRegister("b"))
}

func TestJumpIf(t *testing.T) {
	cpu := NewCpu()

	cpu.instructions = []Instruction{
		{cpy, 41, "a"},
		{inc, "a", nil},
		{inc, "a", nil},
		{dec, "a", nil},
		{jnz, "a", 2},
		{dec, "a", nil},
	}

	cpu.Run()
	assert.Equal(t, 42, cpu.GetRegister("a"))
}

func TestCopyVariable(t *testing.T) {
	cpu := NewCpu()

	cpu.instructions = []Instruction{
		{cpy, 41, "a"},
		{cpy, "a", "b"},
	}

	cpu.Run()
	assert.Equal(t, 41, cpu.GetRegister("b"))
}

func TestJumpIfInteger(t *testing.T) {
	cpu := NewCpu()

	cpu.instructions = []Instruction{
		{cpy, 41, "a"},
		{jnz, 1, 2},
		{inc, "a", nil},
	}

	cpu.Run()
	assert.Equal(t, 41, cpu.GetRegister("a"))
}

func TestToggleInc(t *testing.T) {
	cpu := NewCpu()

	cpu.instructions = []Instruction{
		{cpy, 41, "a"},
		{tgl, 1, nil},
		{inc, "a", nil},
	}

	cpu.Run()
	assert.Equal(t, 40, cpu.GetRegister("a"))
}

func TestToggleDec(t *testing.T) {
	cpu := NewCpu()

	cpu.instructions = []Instruction{
		{cpy, 41, "a"},
		{tgl, 1, nil},
		{dec, "a", nil},
	}

	cpu.Run()
	assert.Equal(t, 42, cpu.GetRegister("a"))
}

func TestToggleOutside(t *testing.T) {
	cpu := NewCpu()

	cpu.instructions = []Instruction{
		{cpy, 41, "a"},
		{tgl, 2, nil},
		{inc, "a", nil},
	}

	cpu.Run()
	assert.Equal(t, 42, cpu.GetRegister("a"))
}

func TestToggleJump(t *testing.T) {
	cpu := NewCpu()

	cpu.instructions = []Instruction{
		{cpy, 41, "a"},
		{tgl, 1, nil},
		{jnz, 42, "a"},
	}

	cpu.Run()
	assert.Equal(t, 42, cpu.GetRegister("a"))
}

func TestToggleCopy(t *testing.T) {
	cpu := NewCpu()

	cpu.instructions = []Instruction{
		{cpy, 41, "a"},
		{tgl, 1, nil},
		{cpy, 42, "a"},
	}

	cpu.Run()
	assert.Equal(t, 41, cpu.GetRegister("a"))
}

func TestToggleToggle(t *testing.T) {
	cpu := NewCpu()

	cpu.instructions = []Instruction{
		{cpy, 0, "a"},
		{cpy, 1, "b"},
		{tgl, "a", nil},
		{dec, "b", nil},
		{jnz, "b", -2},
	}

	cpu.Run()
	assert.Equal(t, 0, cpu.GetRegister("a"))
}

func TestSkipInvalid(t *testing.T) {
	cpu := NewCpu()

	cpu.instructions = []Instruction{
		{tgl, 1, nil},
		{jnz, 2, 1},
	}

	cpu.Run()
	assert.Equal(t, 0, cpu.GetRegister("1"))
}

func TestParseProgram(t *testing.T) {
	cpu := NewCpu()

	program := `cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a`

	cpu.Parse(strings.NewReader(program))
	expected := []Instruction{
		{cpy, 41, "a"},
		{inc, "a", nil},
		{inc, "a", nil},
		{dec, "a", nil},
		{jnz, "a", 2},
		{dec, "a", nil},
	}

	assert.Equal(t, expected, cpu.instructions)
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

	assert.Equal(t, 42, cpu.GetRegister("a"))
}

func TestToggleProgram(t *testing.T) {
	cpu := NewCpu()

	program := `cpy 2 a
tgl a
tgl a
tgl a
cpy 1 a
dec a
dec a`

	cpu.Execute(strings.NewReader(program))

	assert.Equal(t, 3, cpu.GetRegister("a"))
}
