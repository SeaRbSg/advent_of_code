#!/usr/bin/env ruby -w

require "./utils.rb"

class Array
  def minmax_diff
    m, n = self.minmax
    n - m
  end
end

class Problem02a
  def run input
    lines_of_numbers(input).map(&:minmax_diff).sum
  end
end

class Problem02b < Problem02a
  def run input
    lines_of_numbers(input).map { |a| # should use triangle enumeration
      min, max = a.select { |n| a.any? { |m| n != m && ((((m / n) * n) == m) || (((n / m) * m) == n)) } }.minmax
      max / min
    }.sum
  end
end

if __FILE__ == $0 then
  if ARGV.empty? then
    require "minitest/autorun"

    class Test02 < Minitest::Test
      def test_a
        input = "5 1 9 5\n7 5 3\n2 4 6 8\n"

        assert_equal 18, Problem02a.new.run(input)
      end

      def test_b
        input = "5 9 2 8\n9 4 7 3\n3 8 6 5"

        assert_equal 9, Problem02b.new.run(input)
      end
    end
  else
    input = ARGF.read.chomp
    p Problem02a.new.run input
    p Problem02b.new.run input
  end
end
