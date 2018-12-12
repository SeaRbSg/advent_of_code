#!/usr/bin/env ruby -w

INF = 1 / 0.0

initial = nil

map = Hash.new "."

input = if ARGV.empty? then
          DATA
        else
          ARGF
        end

input.each_line do |line|
  case line
  when /initial state: (.+)/ then
    initial = $1
  when /^([\.\#]+) => ([\.\#])/ then
    map[$1] = $2
  when /^$/ then
    # do nothing
  else
    warn "unparsed: #{line.chomp}"
  end
end

def run state, map, iters
  state = state.dup
  orig  = state.dup

  seen = Hash.new 0

  offset = -3
  state.prepend "..."
  state << ("." * 1000)
  state.sub!(/#\.{4,}$/, '#...')

  iters.times do |gen|
    ns = state.dup

    (0..state.size-3).each do |idx|
      ns[idx+2] = map[state[idx, 5]]
    end

    state = ns

    idx = state.index "#"
    case
    when idx < 3 then
      diff = 3 - idx
      state.prepend "." * diff
      offset -= diff
    when idx > 4 then
      diff = idx - 3
      state.delete_prefix! "." * diff
      offset += diff
    end

    # cheating... I don't care... I'm tired
    state << ("." * 1000)
    state.sub!(/#\.{4,}$/, '#...')

    score = state.chars.zip(offset..INF).find_all { |c,_| c == "#" }.map(&:last).sum
    # $stderr.puts "%8d: score = %5d off = %4d state = %s" % [gen+1, score, offset, state]

    key = state.dup
    if seen[key] > 5 then
      warn "pattern detected! (maybe)"

      break
    end
    seen[key] += 1

    break if state == orig
  end

  warn "done"

  return offset, state
end

offset, state = run initial, map, 20
p state.chars.zip(offset..INF).find_all { |c,_| c == "#" }.map(&:last).sum

t0 = Time.now
offset, state = run initial, map, 110_000
p offset, state
p state.chars.zip(offset..INF).find_all { |c,_| c == "#" }.map(&:last).sum
p Time.now - t0

# 12b: solved in mathematica:
#
# In[12]:= FindFormula[{{134, 11873}, {135, 11959}, {136, 12045}, {137,
#    12131}, {138, 12217}, {139, 12303}, {140, 12389}, {141,
#    12475}, {142, 12561}, {143, 12647}, {144, 12733}, {145,
#    12819}, {146, 12905}}, x]
#
# Out[12]= 349. + 86. x

def f x
  349 + 86 * x
end

p f 50_000_000_000

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
