require "./scrambler.rb"

instructions = File.readlines(ARGV[0])

s = Scrambler.new ARGV[1], instructions

s.scramble

puts s.str
