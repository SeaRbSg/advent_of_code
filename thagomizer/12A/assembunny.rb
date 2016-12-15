instructions = File.readlines(ARGV[0])

registers = Hash.new(0)

pc = 0
while pc < instructions.length do
  instruction = instructions[pc]
  case instruction
  when /cpy (\d+) (\w)/
    registers[$2] = $1.to_i
    pc += 1
  when /cpy (\w) (\w)/
    registers[$2] = registers[$1]
    pc += 1
  when /inc (\w)/
    registers[$1] += 1
    pc += 1
  when /dec (\w)/
    registers[$1] -= 1
    pc += 1
  when /jnz (\d+) (-?\w+)/
    if $1.to_i != 0
      pc += $2.to_i
    else
      pc += 1
    end
  when /jnz ([a-d]) (-?\w+)/
    if registers[$1] != 0
      pc += $2.to_i
    else
      pc += 1
    end
  else
    raise "unknown instruction #{instruction}"
  end
end

puts registers
