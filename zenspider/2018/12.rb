#!/usr/bin/env ruby -w

state = nil

# A note like ..#.. => . means that a pot that contains a plant but
# with no plants within two pots of it will not have a plant in it
# during the next generation.

# A note like ##.## => . means that an empty pot with two plants on
# each side of it will remain empty in the next generation.

# A note like .##.# => # means that a pot has a plant in a given
# generation if, in the previous generation, there were plants in that
# pot, the one immediately to the left, and the one two pots to the
# right, but not in the ones immediately to the right and two to the
# left.

map = Hash.new "."

input = if ARGV.empty? then
          DATA
        else
          ARGF
        end

input.each_line do |line|
  case line
  when /initial state: (.+)/ then
    state = $1
  when /^([\.\#]+) => ([\.\#])/ then
    map[$1] = $2
  when /^$/ then
    # do nothing
  else
    warn "unparsed: #{line.chomp}"
  end
end

offset = -3
state[0,0] = "..."
state << ("." * 20)

states = [state.dup]

# puts "%2d: %s" % [0, state]
20.times do |gen|
  ns = state.dup

  (0..state.size-3).each do |idx|
    ns[idx+2] = map[state[idx, 5]]
  end

  state = ns

  if state[0, 3].include? "#" then
    state[0,0] = "..."
    offset -= 3
    warn "offset... #{offset}"
  end

  states << state.dup

  # puts "%2d: %s" % [gen+1, state]
end

puts states.last

p states.last.chars.zip(offset..999).find_all { |c,_| c == "#" }.minmax

p states.last.chars.zip(offset..999).find_all { |c,_| c == "#" }.map(&:last).sum


__END__
initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #
