#!/usr/bin/env ruby -w

require "../2017/utils.rb"

# ## --- Day 4: Secure Container ---
#
# You arrive at the Venus fuel depot only to discover it's protected by a
# password.  The Elves had written the password on a sticky note, but someone
# _threw it out_ ((Look on the bright side - isn't it more secure if nobody
# knows the password?)).
#
# However, they do remember a few key facts about the password:
#
# * It is a six-digit number.
# * The value is within the range given in your puzzle input.
# * Two adjacent digits are the same (like `22` in `1_22_345`).
# * Going from left to right, the digits _never decrease_; they only ever
#   increase or stay the same (like `111123` or `135679`).
#
# Other than the range rule, the following are true:
#
# * `111111` meets these criteria (double `11`, never decreases).
# * `2234_50_` does not meet these criteria (decreasing pair of digits `50`).
# * `123789` does not meet these criteria (no double).
#
# _How many different passwords_ within the range given in your puzzle input
# meet these criteria?

class Problem04a
  def filter n
    ds = n.digits.reverse
    ds.size == 6 && ds.uniq.size < 6 && ds.sort == ds
  end

  def run input
    a, b = input.split(/-/).map(&:to_i)

    ((a+1)...b).find_all { |n| filter n }.size
  end
end

class Problem04b < Problem04a
  def filter n
    ds = n.digits.reverse

    (ds.size == 6 &&
     ds.uniq.size < 6 &&
     ds.sort == ds &&
     ds.chunk_while(&:==).to_a.any? { |a| a.size == 2 })
  end
end

if ARGV.empty? then
  require "minitest/autorun"

  class Test04 < Minitest::Test
    def test_a
      o = Problem04a.new

      assert o.filter(111111), "111111"
      refute o.filter(223450), "223450"
      refute o.filter(123789), "123789"
    end

    def test_b
      o = Problem04b.new

      assert o.filter(112233), "112233"
      refute o.filter(123444), "123444"
      assert o.filter(111122), "111122"
    end
  end
else
  input = ARGF.read.chomp
  p Problem04a.new.run input
  p Problem04b.new.run input
end
