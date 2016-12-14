require './bot.rb'

class Output
  def initalize
    @value = nil
  end

  def receive_chip value
    @value = value
  end
end

value_regex = /value (\d+) goes to bot (\d+)/
wiring_regex  = /bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)/

instructions = File.readlines(ARGV[0])
test_condition = [ARGV[1].to_i, ARGV[2].to_i].sort

bots    = Hash.new { |h, k| h[k] = Bot.new }
outputs = Hash.new { |h, k| h[k] = Output.new }

instructions.each do |instruction|
  case instruction
  when value_regex
    bots[$2].receive_chip($1.to_i)
  when wiring_regex
    bots[$1].low  = ($2 == "bot" ? bots[$3] : outputs[$3])
    bots[$1].high = ($4 == "bot" ? bots[$5] : outputs[$5])
  else
    puts instruction
    raise "ERROR"
  end
end

7.times do
  bots.each do |k, v|
    if v.chips == test_condition
      puts "#{k}"
      exit
    end
    v.tick
  end
end
