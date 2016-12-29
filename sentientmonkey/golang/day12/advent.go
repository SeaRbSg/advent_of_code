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
	registers map[string]int
}

type Instruction struct {
	op, x, y string
}

func NewCpu() *Cpu {
	return &Cpu{make(map[string]int)}
}

func (cpu *Cpu) Execute(reader io.Reader) {
	instructions := cpu.Parse(reader)
	cpu.Run(instructions)
}

func (cpu *Cpu) Parse(reader io.Reader) []Instruction {
	instructions := []Instruction{}
	scanner := bufio.NewScanner(reader)
	for scanner.Scan() {
		line := strings.Split(scanner.Text(), " ")
		instruction := Instruction{op: line[0], x: line[1]}

		if len(line) > 2 {
			instruction.y = line[2]
		}

		instructions = append(instructions, instruction)
	}

	return instructions
}

func (cpu *Cpu) Run(instructions []Instruction) {
	i := 0
	for i < len(instructions) {
		ins := instructions[i]
		//fmt.Printf("%d: %q\n", i, ins)
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
				i += y
				continue
			}
		}
		i++
	}
}

func (cpu *Cpu) Value(variable string) int {
	if value, err := strconv.Atoi(variable); err == nil {
		return value
	}

	return cpu.registers[variable]
}

func (cpu *Cpu) Copy(value int, register string) {
	cpu.registers[register] = value
}

func (cpu *Cpu) Increase(register string) {
	cpu.registers[register]++
}

func (cpu *Cpu) Decrease(register string) {
	cpu.registers[register]--
}
func main() {
	cpu := NewCpu()
	reader := bufio.NewReader(os.Stdin)
	cpu.Execute(reader)
	fmt.Println(cpu.registers["a"])
}
