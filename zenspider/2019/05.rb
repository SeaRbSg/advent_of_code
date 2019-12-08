#!/usr/bin/env ruby -w

require "../2017/utils.rb"
require "./int_code.rb"

class Problem05a
  def run input
    int = IntCode.new input
    int.input << 1
    int.run
    int.output
  end
end

class Problem05b < Problem05a
  def run input
    int = IntCode.new input
    int.input << 5
    int.run
    int.output
  end
end

if ARGV.empty? then
  require "minitest/autorun"

  class Test05 < Minitest::Test
    def test_a
      skip
    end

    def assert_int output, program, input
      int = IntCode.new program
      int.input << input
      int.run
      assert_equal output, int.output
    end

    def test_b
      assert_int [1], "3,9,8,9,10,9,4,9,99,-1,8", 8
      assert_int [0], "3,9,8,9,10,9,4,9,99,-1,8", 7

      assert_int [0], "3,9,7,9,10,9,4,9,99,-1,8", 8
      assert_int [1], "3,9,7,9,10,9,4,9,99,-1,8", 7

      assert_int [1], "3,3,1108,-1,8,3,4,3,99", 8
      assert_int [0], "3,3,1108,-1,8,3,4,3,99", 7

      assert_int [0], "3,3,1107,-1,8,3,4,3,99", 8
      assert_int [1], "3,3,1107,-1,8,3,4,3,99", 7
    end
  end
else
  input = ARGF.read.chomp
  puts Problem05a.new.run input
  puts
  puts Problem05b.new.run input
end
