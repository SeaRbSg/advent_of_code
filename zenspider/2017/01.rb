#!/usr/bin/env ruby -w

class Problem01a
  def run input
    o = offset input
    a = input.chars.to_a
    b = a.dup.cycle(2).drop o
    a.zip(b).inject(0) { |acc, (n, m)| acc + (n == m ? n.to_i : 0) }
  end

  def offset _
    1
  end
end

if __FILE__ == $0 then
  if ARGV.empty? then
    require "minitest/autorun"

    class Test01 < Minitest::Test
      def test_a
        assert_equal 3, Problem01a.new.run("1122")
        assert_equal 4, Problem01a.new.run("1111")
        assert_equal 0, Problem01a.new.run("1234")
        assert_equal 9, Problem01a.new.run("91212129")
      end
    end
  else
    input = ARGF.read.chomp
    p Problem01a.new.run input
  end
end
