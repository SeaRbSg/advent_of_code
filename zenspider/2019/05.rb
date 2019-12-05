#!/usr/bin/env ruby -w

require "../2017/utils.rb"


class IntCode
  attr_accessor :mem
  attr_accessor :pos
  attr_accessor :input
  attr_accessor :output
  attr_accessor :done

  def initialize input
    self.mem     = input.chomp.split(/,/).map(&:to_i)
    self.pos     = 0
    self.input   = []
    self.output  = []
    self.done    = false
  end

  def step
    inst = "%05d" % mem[pos]

    _mc, mb, ma, op = inst[0], inst[1], inst[2], inst[3..4]

    case op
    when "99" then # HALT
      self.done = true
    when "01" then # ADD
      a, b, c = mem[pos+1, 3]

      n1 = ma == "0" ? mem[a] : a
      n2 = mb == "0" ? mem[b] : b
      n3 = c

      mem[n3] = n1 + n2

      self.pos += 4
    when "02" then # MUL
      a, b, c = mem[pos+1, 3]

      n1 = ma == "0" ? mem[a] : a
      n2 = mb == "0" ? mem[b] : b
      n3 = c

      mem[n3] = n1 * n2

      self.pos += 4
    when "03" then # READ
      a, = mem[pos+1, 1]

      mem[a] = input.shift

      self.pos += 2
    when "04" then # WRITE
      a, = mem[pos+1, 1]

      output << mem[a]

      self.pos += 2
    when "05" then # JTRUE
      a, b = mem[pos+1, 2]

      n1 = ma == "0" ? mem[a] : a
      n2 = mb == "0" ? mem[b] : b

      self.pos = n1.zero? ? pos + 3 : n2
    when "06" then # JFALSE
      a, b = mem[pos+1, 2]

      n1 = ma == "0" ? mem[a] : a
      n2 = mb == "0" ? mem[b] : b

      self.pos = n1.zero? ? n2 : pos + 3
    when "07" then # LT
      a, b, c = mem[pos+1, 3]

      n1 = ma == "0" ? mem[a] : a
      n2 = mb == "0" ? mem[b] : b
      n3 = c

      mem[n3] = n1 < n2 ? 1 : 0

      self.pos += 4
    when "08" then # EQ
      a, b, c = mem[pos+1, 3]

      n1 = ma == "0" ? mem[a] : a
      n2 = mb == "0" ? mem[b] : b
      n3 = c

      mem[n3] = n1 == n2 ? 1 : 0
      self.pos += 4
    else
      abort "BAD OP!: #{inst}"
    end
  end

  def run
    step until done
  end
end

class Problem05a
  def run input
    int = IntCode.new input
    int.input << 1
    int.run
    puts int.output
  end
end

class Problem05b < Problem05a
  def run input
    int = IntCode.new input
    int.input << 5
    int.run
    puts int.output
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
  p Problem05a.new.run input
  puts
  p Problem05b.new.run input
end
