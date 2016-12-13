require './screen.rb'

screen = Screen.new

instructions = File.readlines(ARGV[0])

instructions.each do |instruction|
  case instruction
  when /rect (\d+)x(\d+)/
    screen.rect $1.to_i, $2.to_i
  when /rotate column x=(\d+) by (\d+)/
    screen.rotate_column $1.to_i, $2.to_i
  when /rotate row y=(\d+) by (\d+)/
    screen.rotate_row $1.to_i, $2.to_i
  end
end

puts screen.count_pixels
puts screen.to_s
