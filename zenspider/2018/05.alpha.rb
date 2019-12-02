#!/usr/bin/env ruby -w

def time
  t0 = Time.now
  x = yield
  p Time.now - t0
  x
end

input = ARGF.read.chomp

REGEX = Regexp.new(
  (?a..?z)
    .flat_map { |x| y = x.upcase; ["#{x}#{y}", "#{y}#{x}"] }
    .join(?|)
)

def react(polymer)
  true while polymer.gsub!(REGEX, "")
  polymer
end

# Part One
x = time { react(input).size }
p x
puts

# Part Two
x = (?a..?z)
x = time { x.map {|unit| input.gsub(/#{unit}/i, "") } }
x = time { x.map {|polymer| react(polymer)          } }
x = time { x.map {|reacted| reacted.size            } }
x = time { x.min                                      }
p x
