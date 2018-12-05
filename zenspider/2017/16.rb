#!/usr/bin/env ruby -w

class Array
  def swap a, b
    ary = self.dup
    ary[a], ary[b] = ary[b], ary[a]
    ary
  end

  def swap_values a, b
    self.swap self.index(a), self.index(b)
  end
end

class Problem16a
  def run input
    moves = input.split(",")
    setup = ("a".."p").to_a

    dance setup, moves
  end

  def dance setup, moves
    moves.reduce(setup) { |ary, move|
      case move
      when /s(\d+)/ then
        n = $1.to_i
        ary.rotate(-n)
      when /x(\d+)\/(\d+)/ then
        a, b = $1.to_i, $2.to_i
        ary.swap a, b
      when /p(\w)\/(\w)/ then
        a, b = $1, $2
        ary.swap_values a, b
      end
    }
  end
end

class Problem16b < Problem16a
  def dance setup, moves
    1_000.times.reduce(setup) { |ary, n|
      $stderr.print "." if n % 100 == 0
      super ary, moves
    }
  end
end

if __FILE__ == $0 then
  if ARGV.empty? then
    require "minitest/autorun"

    class Test16 < Minitest::Test
      def test_a
        programs = ("a".."e").to_a
        p programs

        # s1, a spin of size 1: eabcd.
        programs = programs.rotate(-1)
        p programs

        # x3/4, swapping the last two programs: eabdc.
        programs = programs.swap 3, 4
        p programs

        # pe/b, swapping programs e and b: baedc.
        programs = programs.swap_values "e", "b"
        p programs

        flunk
      end

      def test_b
        skip
      end
    end
  else
    input = ARGF.read.chomp
    p Problem16a.new.run input
    p Problem16b.new.run input
  end
end
