package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

type Cpu struct {
	registers    map[string]int
	instructions []Instruction
	counter      int
	lastSignal   int
	debug        bool
}

type Operation string

const (
	cpy Operation = "cpy"
	inc Operation = "inc"
	dec Operation = "dec"
	jnz Operation = "jnz"
	tgl Operation = "tgl"
	out Operation = "out"
)

type Instruction struct {
	op   Operation
	x, y string
}

func NewCpu() *Cpu {
	return &Cpu{
		registers:    make(map[string]int),
		instructions: []Instruction{},
		counter:      0,
		lastSignal:   -1,
		debug:        false,
	}
}

func (cpu *Cpu) Execute(reader io.Reader) {
	cpu.Parse(reader)
	cpu.Run()
}

func (cpu *Cpu) ToOperation(value string) Operation {
	switch value {
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
	case "out":
		return out
	}

	panic(fmt.Errorf("Unkown operation: %s", value))
}

func (cpu *Cpu) Parse(reader io.Reader) {
	scanner := bufio.NewScanner(reader)
	for scanner.Scan() {
		line := strings.Split(scanner.Text(), " ")
		instruction := Instruction{op: cpu.ToOperation(line[0]), x: line[1]}

		if len(line) > 2 {
			instruction.y = line[2]
		}

		cpu.instructions = append(cpu.instructions, instruction)
	}
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
		case out:
			x := cpu.Value(ins.x)
			if !cpu.Output(x) {
				return
			}
		}
		cpu.counter++
	}
}

func (cpu *Cpu) Value(variable string) int {
	if value, err := strconv.Atoi(variable); err == nil {
		return value
	}

	return cpu.registers[variable]
}

func (cpu *Cpu) ValidRegister(register string) bool {
	return register >= "a" && register <= "d"
}

func (cpu *Cpu) Output(value int) bool {
	switch cpu.lastSignal {
	case 1:
		if value != 0 {
			return false
		}
	case 0:
		if value != 1 {
			return false
		}
	}
	cpu.lastSignal = value
	return true
}

func (cpu *Cpu) Copy(value int, register string) {
	if cpu.ValidRegister(register) {
		cpu.registers[register] = value
	}
}

func (cpu *Cpu) Increase(register string) {
	cpu.registers[register]++
}

func (cpu *Cpu) Decrease(register string) {
	cpu.registers[register]--
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

func main() {
	reader := bufio.NewReader(os.Stdin)
	cpu := NewCpu()
	cpu.Parse(reader)
	startingInstructions := cpu.instructions
	startingA := 0
	for i := 0; true; i++ {
		startingA = i
		fmt.Println(i)
		cpu := NewCpu()
		cpu.debug = true
		cpu.registers["a"] = startingA
		cpu.instructions = startingInstructions
		cpu.Run()
	}
	fmt.Println(startingA)
}
