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
	debug        bool
}

type Instruction struct {
	op, x, y string
}

func NewCpu() *Cpu {
	return &Cpu{make(map[string]int), []Instruction{}, 0, false}
}

func (cpu *Cpu) Execute(reader io.Reader) {
	cpu.Parse(reader)
	cpu.Run()
}

func (cpu *Cpu) Parse(reader io.Reader) {
	scanner := bufio.NewScanner(reader)
	for scanner.Scan() {
		line := strings.Split(scanner.Text(), " ")
		instruction := Instruction{op: line[0], x: line[1]}

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

func (cpu *Cpu) Value(variable string) int {
	if value, err := strconv.Atoi(variable); err == nil {
		return value
	}

	return cpu.registers[variable]
}

func (cpu *Cpu) ValidRegister(register string) bool {
	return register >= "a" && register <= "d"
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
	cpu := NewCpu()
	cpu.registers["a"] = 12
	reader := bufio.NewReader(os.Stdin)
	cpu.Execute(reader)
	fmt.Println(cpu.registers["a"])
}
