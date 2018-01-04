#!/usr/bin/env ruby -w

require "pp"
require "stringio"

class Problem08a
  attr_accessor :mem

  def initialize
    self.mem = Hash.new 0
  end

  def inc reg, amt
    mem[reg] += amt
  end

  def dec reg, amt
    mem[reg] -= amt
  end

  def parse input
    input.lines.each do |line|
      case line
      when /^(\w+) (inc|dec) ([-\d]+) if (\w+) (==|[!<>]=|[<>]) ([-\d]+)/
        reg, dir, amt, if_reg, op, if_val = $1, $2, $3.to_i, $4, $5, $6.to_i
        self.send(dir, reg, amt) if mem[if_reg].send(op, if_val)
      else
        warn "unparsed: #{line.chomp}"
      end
    end
    mem
  end

  def run input
    parse input
    mem.values.max
  end
end

class Problem08b < Problem08a
  attr_accessor :max

  def initialize
    super
    self.max = 0
  end

  def store_max
    self.max = [mem.values.max, max].max
  end

  def inc reg, amt
    super
    store_max
  end

  def dec reg, amt
    super
    store_max
  end

  def run input
    parse input
    self.max
  end
end

if __FILE__ == $0 then
  if ARGV.empty? then
    require "minitest/autorun"

    class Test08 < Minitest::Test
      INPUT_A = ["b inc 5 if a > 1\na inc 1 if b < 5",
                 "c dec -10 if a >= 1\nc inc -20 if c == 10"].join "\n"

      def test_a_parse
        exp = { "a" => 1, "c" => -10 }
        assert_equal exp, Problem08a.new.parse(INPUT_A)
      end

      def test_a_run
        assert_equal 1, Problem08a.new.run(INPUT_A)
      end

      def test_b
        problem = Problem08b.new
        assert_equal 1, problem.run(INPUT_A)
        assert_equal 10, problem.max
      end
    end
  else
    input = ARGF.read
    p Problem08a.new.run input
    p Problem08b.new.run input
  end
end
