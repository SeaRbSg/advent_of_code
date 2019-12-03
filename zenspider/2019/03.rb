#!/usr/bin/env ruby -w

require "../2017/utils.rb"

class Problem03a
  def parse input
    input.lines.map { |l|
      l.split(/,/).flat_map { |coord|
        case coord
        when /R(\d+)/ then [[ 1, 0]] * $1.to_i
        when /L(\d+)/ then [[-1, 0]] * $1.to_i
        when /U(\d+)/ then [[0,  1]] * $1.to_i
        when /D(\d+)/ then [[0, -1]] * $1.to_i
        else
          raise "FUCK #{coord}"
        end
      }
    }
  end

  def walk input
    input.reduce([[0, 0]]) { |coords, (dx, dy)|
      x, y = coords.last
      coords << [x+dx, y+dy]
    }.drop 1
  end

  def run input
    line_a, line_b = parse input

    line_a = walk line_a
    line_b = walk line_b

    x, y = (line_a & line_b).min_by { |dx, dy| dx.abs + dy.abs }

    x.abs + y.abs
  end
end

class Problem03b < Problem03a
end

if ARGV.empty? then
  require "minitest/autorun"

  class Test03 < Minitest::Test
    INPUT = "R8,U5,L5,D3\nU7,R6,D4,L4\n"

    def test_parse
      act = Problem03a.new.parse INPUT

      exp = [[[8, 0], [0, 5], [-5, 0], [0, -3]],
             [[0, 7], [6, 0], [0, -4], [-4, 0]]]

      exp = [[[1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0],
              [0, 1], [0, 1], [0, 1], [0, 1], [0, 1],
              [-1, 0], [-1, 0], [-1, 0], [-1, 0], [-1, 0],
              [0, -1], [0, -1], [0, -1]],

             [[0, 1], [0, 1], [0, 1], [0, 1], [0, 1], [0, 1], [0, 1],
              [1, 0], [1, 0], [1, 0], [1, 0], [1, 0], [1, 0],
              [0, -1], [0, -1], [0, -1], [0, -1],
              [-1, 0], [-1, 0], [-1, 0], [-1, 0]]]

      assert_equal exp, act
    end

    def test_walk
      o = Problem03a.new
      i = o.parse INPUT
      w = o.walk i.first

      exp = [[1, 0], [2, 0], [3, 0], [4, 0], [5, 0], [6, 0], [7, 0], [8, 0],
             [8, 1], [8, 2], [8, 3], [8, 4], [8, 5],
             [7, 5], [6, 5], [5, 5], [4, 5], [3, 5],
             [3, 4], [3, 3], [3, 2]]

      assert_equal w.size, exp.size

      assert_equal exp, w
    end

    def test_run_a_1
      assert_equal 6, Problem03a.new.run(INPUT)
    end

    def test_run_a_2
      assert_equal 159, Problem03a.new.run("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83")
    end

    def test_run_a_3
      assert_equal 135, Problem03a.new.run("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
    end

    def test_b
      skip
    end
  end
else
  input = ARGF.read.chomp
  p Problem03a.new.run input
  p Problem03b.new.run input
end
