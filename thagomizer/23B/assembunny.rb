instructions = File.readlines(ARGV[0]).map(&:strip)

def alter instruction
  case instruction
  when /cpy (\w+) (\w+)/
    "jnz #{$1} #{$2}"
  when /inc (\w)/
    "dec #{$1}"
  when /dec (\w)/
    "inc #{$1}"
  when /jnz (\w+) (-?\w+)/
    "cpy #{$1} #{$2}"
  when /tgl ([a-d])/
    "inc #{$1}"
  else
    raise "unknown instruction #{instruction}"
  end
end

registers = Hash.new(0)
registers["a"] = ARGV[1].to_i

pc = 0
while pc < instructions.length do
  instruction = instructions[pc]

  case instruction
  when /mul ([a-d]) ([a-d]) ([a-d])/
    registers[$3] = registers[$1] * registers[$2]
  when /cpy (-?\d+) (\w)/
    registers[$2] = $1.to_i
  when /cpy (\w) (\w)/
    registers[$2] = registers[$1]
  when /inc (\w)/
    registers[$1] += 1
  when /dec (\w)/
    registers[$1] -= 1
  when /jnz (\d+) (-?\d+)/
    if $1.to_i != 0
      pc += $2.to_i
      next
    end
  when /jnz (\d+) ([a-d])/
    if $1.to_i != 0
      pc += registers[$2]
      next
    end
  when /jnz ([a-d]) (-?\d+)/
    if registers[$1] != 0
      pc += $2.to_i
      next
    end
  when /tgl ([a-d])/
    alter_index = registers[$1] + pc

    if instructions[alter_index]
      instructions[alter_index] = alter(instructions[alter_index])
    end
  else
    raise "unknown instruction #{instruction}"
  end
  pc += 1
end

puts instructions
puts registers
