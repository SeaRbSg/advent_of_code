#!/usr/bin/env ruby -w

require "./utils.rb"

class Problem05a
  attr_accessor :jumps, :idx

  def run input
    self.jumps = input.integers

    self.idx = 0
    count = 0
    max = jumps.size

    while idx < max do
      count += 1

      jump = jumps[idx]
      morph
      self.idx += jump
    end

    count
  end

  def morph
    jumps[idx] += 1
  end
end

class Problem05b < Problem05a
  def morph
    jumps[idx] += jumps[idx] > 2 ? -1 : 1
  end
end

if __FILE__ == $0 then
  if ARGV.empty? then
    require "minitest/autorun"

    class Test05 < Minitest::Test
      def test_a
        assert_equal 5, Problem05a.new.run("0 3 0 1 -3")
      end

      def test_b
        assert_equal 10, Problem05b.new.run("0 3 0 1 -3")
      end
    end
  else
    input = ARGF.read.chomp
    p Problem05a.new.run input
    p Problem05b.new.run input
  end
end
