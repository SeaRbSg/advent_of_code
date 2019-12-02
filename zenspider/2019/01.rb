#!/usr/bin/env ruby -w

require "../2017/utils.rb"

class Problem01a
  def fuel mass
    (mass / 3).floor - 2
  end

  def run input
    input.integers.sum { |n| fuel n }
  end
end

class Problem01b < Problem01a
  def fuel mass
    f = (mass / 3).floor - 2

    f > 0 ? f + fuel(f) : 0
  end
end

if ARGV.empty? then
  require "minitest/autorun"

  class Test01 < Minitest::Test
    def test_fuel
      x = Problem01a.new

      assert_equal 2,     x.fuel(12)
      assert_equal 2,     x.fuel(14)
      assert_equal 654,   x.fuel(1969)
      assert_equal 33583, x.fuel(100756)
    end

    def test_a
      input = "12\n14\n1969\n100756\n"

      sum = Problem01a.new.run input
      exp = 2 + 2 + 654 + 33583

      assert_equal exp, sum
    end

    def test_b
      x = Problem01b.new

      assert_equal 2,     x.fuel(12)
      assert_equal 2,     x.fuel(14)
      assert_equal 966,   x.fuel(1969)
      assert_equal 50346, x.fuel(100756)
    end
  end
else
  input = ARGF.read.chomp
  p Problem01a.new.run input
  p Problem01b.new.run input
end
