require 'pp'

class Disk
  def initialize offset, positions, starting_position
    @offset = offset
    @positions = positions
    @starting_position = starting_position
  end

  def position_at_time t
    (@offset + @starting_position + t) % @positions
  end
end

PARSING_REGEXP = /Disc #(\d) has (\d+) positions; at time=0, it is at position (\d+)./

input = File.readlines(ARGV[0])

disks = []

input.each do |line|
  line =~ PARSING_REGEXP
  disks << Disk.new(*[$1, $2, $3].map(&:to_i))
end

t = 0
loop do
  break if disks.all?{ |d | d.position_at_time(t) == 0 }
  t += 1
end

puts t
