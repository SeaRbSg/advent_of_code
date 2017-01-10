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
	registers    map[Variable]int
	instructions []Instruction
	counter      int
	debug        bool
}

type Variable interface{}

type Instruction struct {
	op   string
	x, y Variable
}

func NewCpu() *Cpu {
	return &Cpu{make(map[Variable]int), []Instruction{}, 0, false}
}

func (cpu *Cpu) Execute(reader io.Reader) {
	cpu.Parse(reader)
	cpu.Run()
}

func (cpu *Cpu) Parse(reader io.Reader) {
	scanner := bufio.NewScanner(reader)
	for scanner.Scan() {
		line := strings.Split(scanner.Text(), " ")
		x := cpu.ParseVariable(line[1])
		instruction := Instruction{op: line[0], x: x}

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
		case "cpy":
			x := cpu.Value(ins.x)
			cpu.Copy(x, ins.y)
		case "inc":
			cpu.Increase(ins.x)
		case "dec":
			cpu.Decrease(ins.x)
		case "jnz":
			x := cpu.Value(ins.x)
			if x != 0 {
				y := cpu.Value(ins.y)
				cpu.counter += y
				continue
			}
		case "tgl":
			x := cpu.Value(ins.x)
			addr := cpu.counter + x
			if addr >= 0 && addr < len(cpu.instructions) {
				cpu.instructions[addr] = cpu.Toggle(cpu.instructions[addr])
			}
		}
		cpu.counter++
	}
}

func (cpu *Cpu) Value(variable Variable) int {

	switch v := variable.(type) {
	case string:
		if cpu.ValidRegister(v) {
			return cpu.registers[variable]
		}
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

func (cpu *Cpu) Copy(value int, register Variable) {
	if cpu.ValidRegister(register) {
		cpu.registers[register] = value
	}
}

func (cpu *Cpu) Increase(register Variable) {
	if cpu.ValidRegister(register) {
		cpu.registers[register]++
	}
}

func (cpu *Cpu) Decrease(register Variable) {
	if cpu.ValidRegister(register) {
		cpu.registers[register]--
	}
}

func (cpu *Cpu) Toggle(ins Instruction) Instruction {
	switch ins.op {
	case "inc":
		return Instruction{op: "dec", x: ins.x}
	case "dec", "tgl":
		return Instruction{op: "inc", x: ins.x}
	case "jnz":
		return Instruction{op: "cpy", x: ins.x, y: ins.y}
	case "cpy":
		return Instruction{op: "jnz", x: ins.x, y: ins.y}
	}
	return ins
}

// ./advent < input.txt  359.90s user 3.05s system 100% cpu 6:00.48 total
func main() {
	defer profile.Start(profile.CPUProfile).Stop()
	cpu := NewCpu()
	cpu.registers["a"] = 12
	reader := bufio.NewReader(os.Stdin)
	cpu.Execute(reader)
	fmt.Println(cpu.registers["a"])
}
