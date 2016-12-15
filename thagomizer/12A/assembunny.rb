instructions = File.readlines(ARGV[0])

registers = Hash.new(0)

pc = 0
while pc < instructions.length do
  instruction = instructions[pc]
  case instruction
  when /cpy (\d+) (\w)/
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
  when /jnz ([a-d]) (-?\d+)/
    if registers[$1] != 0
      pc += $2.to_i
      next
    end
  else
    raise "unknown instruction #{instruction}"
  end
  pc += 1
end

puts registers
