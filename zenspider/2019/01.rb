#!/usr/bin/env ruby -w

# ## --- Day 1: The Tyranny of the Rocket Equation ---
#
# Santa has become stranded at the edge of the Solar System while delivering
# presents to other planets! To accurately calculate his position in space,
# safely align his warp drive, and return to Earth in time to save Christmas,
# he needs you to bring him _measurements_ ((If only you had time to grab an
# astrolabe.)) from _fifty stars_.
#
# Collect stars by solving puzzles.  Two puzzles will be made available on
# each day in the Advent calendar; the second puzzle is unlocked when you
# complete the first.  Each puzzle grants _one star_. Good luck!
#
# The Elves quickly load you into a spacecraft and prepare to launch.
#
# At the first Go / No Go poll, every Elf is Go until the Fuel Counter-Upper.
# They haven't determined the amount of fuel required yet.
#
# Fuel required to launch a given _module_ is based on its _mass_.
# Specifically, to find the fuel required for a module, take its mass, divide
# by three, round down, and subtract 2.
#
# For example:
#
# * For a mass of `12`, divide by 3 and round down to get `4`, then subtract 2
#   to get `2`.
# * For a mass of `14`, dividing by 3 and rounding down still yields `4`, so
#   the fuel required is also `2`.
# * For a mass of `1969`, the fuel required is `654`.
# * For a mass of `100756`, the fuel required is `33583`.
#
# The Fuel Counter-Upper needs to know the total fuel requirement.  To find
# it, individually calculate the fuel needed for the mass of each module
# (your puzzle input), then add together all the fuel values.
#
# _What is the sum of the fuel requirements_ for all of the modules on your
# spacecraft?

class Problem01a
  def run input
  end
end

class Problem01b < Problem01a
end

if ARGV.empty? then
  require "minitest/autorun"

  class Test01 < Minitest::Test
    def test_a
      flunk
    end

    def test_b
      skip
    end
  end
else
  input = ARGF.read.chomp
  p Problem01a.new.run input
  p Problem01b.new.run input
end
