require "./scrambler.rb"

instructions = File.readlines(ARGV[0]).reverse

s = Scrambler.new String.new(ARGV[1]), instructions

s.unscramble

puts s.str
