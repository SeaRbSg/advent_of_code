#!/usr/bin/env ruby -w

class ProblemNNa
  def run input
  end
end

class ProblemNNb < ProblemNNa
end

module Enumerable
  def flip
    map(&:reverse)
  end

  def rotate90
    transpose.flip
  end

  def rotate180
    rotate90.rotate90
  end

  def rotate270
    flip.transpose
  end

  def convolute
    a = map(&:dup) # deep copy
    [a,           a.flip,
     a.rotate90,  a.rotate90.flip,
     a.rotate180, a.rotate180.flip,
     a.rotate270, a.rotate270.flip]
  end

  def split23
    size  = Math.sqrt(self.size).to_i
    lines = each_slice(size)
    side  = size.even? ? 2 : 3

    lines.each_slice(side).flat_map { |a, *rest|
      a.each_slice(side).zip(*rest.map { |e| e.each_slice(side) }).map(&:flatten)
    }
  end
end

class String
  def split23
    chars.to_a.split23.map(&:join)
  end
end

pp "0123".split23                                 # 2x2
pp "012345678".split23                            # 3x3
pp "0123456789ABCDEF".split23                     # 4x4
pp "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".split23 # 6x6
pp "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".chars.cycle.take(81).join.split23

s = "0123456789ABCDEF"
pp s.split23
pp s.chars.to_a.split23

s = "###.##..#"

pp s.chars.to_a.split23.map { |a| a.each_slice(3).to_a } # .convolute
pp s.chars.to_a.split23.map { |a| a.each_slice(3).to_a }.convolute.map(&:join)

File.foreach "input/21.txt" do |l|
  case l
  when %r%([#/.]+) => ([#/.]+)% then
    from, to = $1.delete("/"), $2.delete("/")

    # p :from => from, :to => to

  else
    warn "unparsed: #{l.chomp}"
  end
end


# if __FILE__ == $0 then
#   if ARGV.empty? then
#     require "minitest/autorun"
#
#     class TestNN < Minitest::Test
#       def test_a
#         flunk
#       end
#
#       def test_b
#         skip
#       end
#     end
#   else
#     input = ARGF.read.chomp
#     p ProblemNNa.new.run input
#     p ProblemNNb.new.run input
#   end
# end
#
# "###/###/###".delete("/").tr("#.", "10").to_i 2
#
# s = "#../.../..."
# s.tr("/#.", "\n10")


# --- Day 21: Fractal Art ---
#
# You find a program trying to generate some art. It uses a strange
# process that involves repeatedly enhancing the detail of an image
# through a set of rules.
#
# The image consists of a two-dimensional square grid of pixels that
# are either on (#) or off (.). The program always begins with this
# pattern:
#
# .#.
# ..#
# ###
#
# Because the pattern is both 3 pixels wide and 3 pixels tall, it is
# said to have a size of 3.
#
# Then, the program repeats the following process:
#
# If the size is evenly divisible by 2, break the pixels up into 2x2
# squares, and convert each 2x2 square into a 3x3 square by following
# the corresponding enhancement rule. Otherwise, the size is evenly
# divisible by 3; break the pixels up into 3x3 squares, and convert
# each 3x3 square into a 4x4 square by following the corresponding
# enhancement rule. Because each square of pixels is replaced by a
# larger one, the image gains pixels and so its size increases.
#
# The artist's book of enhancement rules is nearby (your puzzle
# input); however, it seems to be missing rules. The artist explains
# that sometimes, one must rotate or flip the input pattern to find a
# match. (Never rotate or flip the output pattern, though.) Each
# pattern is written concisely: rows are listed as single units,
# ordered top-down, and separated by slashes. For example, the
# following rules correspond to the adjacent patterns:
#
# ../.#  =  ..
#           .#
#
#                 .#.
# .#./..#/###  =  ..#
#                 ###
#
#                         #..#
# #..#/..../#..#/.##.  =  ....
#                         #..#
#                         .##.
#
# When searching for a rule to use, rotate and flip the pattern as
# necessary. For example, all of the following patterns match the same
# rule:
#
# .#.   .#.   #..   ###
# ..#   #..   #.#   ..#
# ###   ###   ##.   .#.
#
# Suppose the book contained the following two rules:
#
# ../.# => ##./#../...
# .#./..#/### => #..#/..../..../#..#
#
# As before, the program begins with this pattern:
#
# .#.
# ..#
# ###
#
# The size of the grid (3) is not divisible by 2, but it is divisible
# by 3. It divides evenly into a single square; the square matches the
# second rule, which produces:
#
# #..#
# ....
# ....
# #..#
#
# The size of this enhanced grid (4) is evenly divisible by 2, so that
# rule is used. It divides evenly into four squares:
#
# #.|.#
# ..|..
# --+--
# ..|..
# #.|.#
#
# Each of these squares matches the same rule (../.# => ##./#../...),
# three of which require some flipping and rotation to line up with
# the rule. The output for the rule is the same in all four cases:
#
# ##.|##.
# #..|#..
# ...|...
# ---+---
# ##.|##.
# #..|#..
# ...|...
#
# Finally, the squares are joined into a new grid:
#
# ##.##.
# #..#..
# ......
# ##.##.
# #..#..
# ......
#
# Thus, after 2 iterations, the grid contains 12 pixels that are on.
#
# How many pixels stay on after 5 iterations?
