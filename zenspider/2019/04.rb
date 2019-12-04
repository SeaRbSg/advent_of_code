#!/usr/bin/env ruby -w

require "../2017/utils.rb"

class Problem04a
  def filter n
    ds = n.digits.reverse
    ds.uniq.size < 6 && ds.sort == ds
  end

  def run input
    a, b = input.split(/-/).map(&:to_i)

    (a..b).count { |n| filter n }
  end
end

class Problem04b < Problem04a
  def filter n
    super &&
      n.digits.reverse.chunk_while(&:==).to_a.any? { |a| a.size == 2 }
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
