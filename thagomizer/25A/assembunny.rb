require 'pp'
require 'pry'

class Assembunny
  attr_accessor :registers, :instructions, :operations, :clock_signal

  def initialize instructions
    @instructions = instructions
    @operations = []
    @registers = Hash.new(0)
    @pc = 0
    @clock_signal = []
  end

  def parse
    @instructions.each do |instruction|
      case instruction
      when /noop/
        @operations << [:noop]
      when /mul ([a-d]) ([a-d]) ([a-d])/
        @operations << [:mul, $1.to_sym, $2.to_sym, $3.to_sym]
      when /add ([a-d]) ([a-d]) /
        @operations << [:mul, $1.to_sym, $2.to_sym]
      when /mod ([a-d]) (\d+) ([a-d])/
        @operations << [:mod, $1.to_sym, $2.to_i, $3.to_sym]
      when /cpy (-?\d+) (\w)/
        @operations << [:cpy_const, $1.to_i, $2.to_sym]
      when /cpy (\w) (\w)/
        @operations << [:cpy_reg, $1.to_sym, $2.to_sym]
      when /inc (\w)/
        @operations << [:inc, $1.to_sym]
      when /dec (\w)/
        @operations << [:dec, $1.to_sym]
      when /jnz (\d+) (-?\d+)/
        @operations << [:jnz_dd, $1.to_i, $2.to_i]
      when /jnz (\d+) ([a-d])/
        @operations << [:jnz_dr, $1.to_i, $2.to_sym]
      when /jnz ([a-d]) (-?\d+)/
        @operations << [:jnz_rd, $1.to_sym, $2.to_i]
      when /jnz ([a-d]) ([a-d])/
        @operations << [:jnz_rr, $1.to_sym, $2.to_sym]
      when /out ([a-d])/
        @operations << [:out_reg, $1.to_sym]
      when /out (\d+)/
        @operations << [:out_const, $1.to_i]
      else
        raise "unknown instruction #{instruction}"
      end
    end
  end

  def process_instructions
    parse if @operations.empty?

    counter = 0

    while @pc < @operations.length do
      operation = @operations[@pc]

      op, arg_1, arg_2, arg_3 = *operation

      case op
      when :mul
        @registers[arg_3] = @registers[arg_1] * @registers[arg_2]
      when :add
        @registers[arg_2] = @registers[arg_1] + @registers[arg_2]
      when :mod
        @registers[arg_3] = @registers[arg_1] % arg_2
      when :cpy_const
        @registers[arg_2] = arg_1
      when :cpy_reg
        @registers[arg_2] = @registers[arg_1]
      when :inc
        @registers[arg_1] += 1
      when :dec
        @registers[arg_1] -= 1
      when :jnz_dd
        if arg_1 != 0
          @pc += arg_2
          next
        end
      when :jnz_dr
        if arg_1 != 0
          @pc += @registers[arg_2]
          next
        end
      when :jnz_rd
        if @registers[arg_1] != 0
          @pc += arg_2
          next
        end
      when :jnz_rr
        if @registers[arg_1] != 0
          @pc += @registers[arg_2]
          next
        end
      when :noop
        # NOOP
      when :out_const
        break if @clock_signal[-1] == 0 and arg_1 != 1
        break if @clock_signal[-1] == 1 and arg_1 != 0
        @clock_signal << arg_1
      when :out_reg
        val = @registers[arg_1]
        break if @clock_signal[-1] == 0 and val != 1
        break if @clock_signal[-1] == 1 and val != 0
        @clock_signal << val
      else
        raise "unknown instruction #{instruction}"
      end

      @pc += 1
      counter += 1
      break if counter > 1_000_000
    end
  end

  def reset
    @registers = {}
    @clock_signal = []
    @pc = 0
  end
end

instructions = File.readlines(ARGV[0]).map(&:strip)

a = Assembunny.new instructions
a.parse
n = 0
target = Array.new(100)
target.fill { |i| i % 2 }
pp target.join

loop do
  a.reset
  a.registers[:a] = n
  a.process_instructions
  if a.clock_signal.first(100) == target
    puts n
    exit
  end

  n += 1

  exit if n > 1_000_000
end
