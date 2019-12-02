#!/usr/bin/env ruby -w

require "../2017/utils.rb"

class Problem02a
  attr_accessor :ops
  attr_accessor :pos

  def initialize input
    self.ops = input.integers
    self.pos = 0
  end

  def seed noun, verb
    self.pos = 0
    ops[1] = noun
    ops[2] = verb
  end

  def execute
    ops.each_slice(4) do |op, a, b, dst|
      case op
      when 1 then
        n1  = ops[a]
        n2  = ops[b]

        ops[dst] = n1 + n2
      when 2 then
        n1  = ops[a]
        n2  = ops[b]

        ops[dst] = n1 * n2
      when 99 then
        break
      end
      self.pos += 4
    end

    ops
  end

  def run
    execute
  end
end

class Problem02b < Problem02a
  def run
    backup = ops.dup

    (1..99).to_a.permutation(2) do |noun, verb|
      self.ops = backup.dup
      self.seed noun, verb

      result = execute.first

      return noun*100 + verb if result == 19690720
    end

    raise "bad"
  end
end

if ARGV.empty? then
  require "minitest/autorun"

  class Test02 < Minitest::Test
    def assert_execute exp, inp
      assert_equal exp.integers, Problem02a.new(inp).run
    end

    def test_a
      assert_execute "2,0,0,0,99", "1,0,0,0,99"
      assert_execute "2,3,0,_6_,99", "2,3,0,3,99"
      assert_execute "2,4,4,5,99,_9801_", "2,4,4,5,99,0"
      assert_execute "_30_,1,1,4,_2_,5,6,0,99", "1,1,1,4,99,5,6,0,99"
      assert_execute("3500,9,10,70,2,3,11,0,99,30,40,50",
                     "1,9,10,3,2,3,11,0,99,30,40,50")
    end

    def test_b
      # nothing to do
    end
  end
else
  input = ARGF.read.chomp

  cpu = Problem02a.new(input)
  cpu.seed 12, 2
  p cpu.run.first

  p Problem02b.new(input).run
end
