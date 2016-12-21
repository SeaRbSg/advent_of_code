require 'pp'

file = ARGV[0]
min  = ARGV[1].to_i
max  = ARGV[2].to_i

data = File.readlines(file)

data.map! do |line|
  line =~ /(\d+)-(\d+)/
  low  = $1.to_i
  high = $2.to_i

  [low, high]
end

data.sort!

current_high = 0

data.each do |low, high|
  if low > current_high + 1
    puts current_high + 1
    exit
  end

  current_high = high
end
