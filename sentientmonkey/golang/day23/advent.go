package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"

	"github.com/pkg/profile"
)

type Cpu struct {
	registers    [5]int
	instructions []Instruction
	counter      int
	debug        bool
}

type Variable interface{}

type Instruction struct {
	op   Operation
	x, y Variable
}

type Operation int

const (
	cpy Operation = iota
	inc
	dec
	jnz
	tgl
	invalid
)

func NewCpu() *Cpu {
	return &Cpu{[5]int{}, []Instruction{}, 0, false}
}

func (cpu *Cpu) Execute(reader io.Reader) {
	cpu.Parse(reader)
	cpu.Run()
}

func (cpu *Cpu) ToOperation(s string) Operation {
	switch s {
	case "cpy":
		return cpy
	case "inc":
		return inc
	case "dec":
		return dec
	case "jnz":
		return jnz
	case "tgl":
		return tgl
	}

	return invalid
}

func (cpu *Cpu) Parse(reader io.Reader) {
	scanner := bufio.NewScanner(reader)
	for scanner.Scan() {
		line := strings.Split(scanner.Text(), " ")
		x := cpu.ParseVariable(line[1])
		instruction := Instruction{op: cpu.ToOperation(line[0]), x: x}

		if len(line) > 2 {
			y := cpu.ParseVariable(line[2])
			instruction.y = y
		}

		cpu.instructions = append(cpu.instructions, instruction)
	}
}

func (cpu *Cpu) ParseVariable(variable string) Variable {
	if cpu.ValidRegister(variable) {
		return variable
	}

	value, _ := strconv.Atoi(variable)
	return value
}

func (cpu *Cpu) Run() {
	for cpu.counter < len(cpu.instructions) {
		ins := cpu.instructions[cpu.counter]
		if cpu.debug {
			fmt.Printf("registers: %v\n", cpu.registers)
			fmt.Printf("stack: %v\n", cpu.instructions)
			fmt.Printf("%d: %v\n", cpu.counter, ins)
		}
		switch ins.op {
		case cpy:
			x := cpu.Value(ins.x)
			cpu.Copy(x, ins.y)
		case inc:
			cpu.Increase(ins.x)
		case dec:
			cpu.Decrease(ins.x)
		case jnz:
			x := cpu.Value(ins.x)
			if x != 0 {
				y := cpu.Value(ins.y)
				cpu.counter += y
				continue
			}
		case tgl:
			x := cpu.Value(ins.x)
			addr := cpu.counter + x
			if addr >= 0 && addr < len(cpu.instructions) {
				cpu.instructions[addr] = cpu.Toggle(cpu.instructions[addr])
			}
		}
		cpu.counter++
	}
}

func (cpu *Cpu) RegisterIndex(register string) (int, bool) {
	switch register {
	case "a":
		return 0, true
	case "b":
		return 1, true
	case "c":
		return 2, true
	case "d":
		return 3, true
	}

	return 0, false
}

func (cpu *Cpu) SetRegister(register string, value int) {
	if index, ok := cpu.RegisterIndex(register); ok {
		cpu.registers[index] = value
	}
}

func (cpu *Cpu) GetRegister(register string) int {
	if index, ok := cpu.RegisterIndex(register); ok {
		return cpu.registers[index]
	}

	return 0
}

func (cpu *Cpu) Value(variable Variable) int {

	switch v := variable.(type) {
	case string:
		return cpu.GetRegister(v)
	case int:
		return v
	}

	return 0
}

func (cpu *Cpu) ValidRegister(register Variable) bool {
	if r, ok := register.(string); ok {
		return r >= "a" && r <= "d"
	}

	return false
}

func (cpu *Cpu) Copy(value int, variable Variable) {
	if register, ok := variable.(string); ok {
		cpu.SetRegister(register, value)
	}
}

func (cpu *Cpu) Increase(variable Variable) {
	if register, ok := variable.(string); ok {
		if index, ok := cpu.RegisterIndex(register); ok {
			cpu.registers[index]++
		}
	}
}

func (cpu *Cpu) Decrease(variable Variable) {
	if register, ok := variable.(string); ok {
		if index, ok := cpu.RegisterIndex(register); ok {
			cpu.registers[index]--
		}
	}
}

func (cpu *Cpu) Toggle(ins Instruction) Instruction {
	switch ins.op {
	case inc:
		return Instruction{op: dec, x: ins.x}
	case dec, tgl:
		return Instruction{op: inc, x: ins.x}
	case jnz:
		return Instruction{op: cpy, x: ins.x, y: ins.y}
	case cpy:
		return Instruction{op: jnz, x: ins.x, y: ins.y}
	}
	return ins
}

// ./advent < input.txt  78.14s user 0.49s system 98% cpu 1:19.45 total
func main() {
	defer profile.Start(profile.CPUProfile).Stop()
	cpu := NewCpu()
	cpu.SetRegister("a", 12)
	reader := bufio.NewReader(os.Stdin)
	cpu.Execute(reader)
	fmt.Println(cpu.GetRegister("a"))
}
