#!/usr/bin/env ruby -w

require "../2017/utils.rb"

class Planets
  attr_accessor :planets

  def initialize input
    self.planets = input
      .split(/\n/)
      .map { |s| s.split(")").reverse }
      .to_h
  end

  def run_a
    planets.keys.sum { |k| count k }
  end

  def run_b
    path_length("YOU", "SAN") - 2
  end

  def count planet
    sub = planets[planet]            # => "COM", nil, "B", ...

    if sub then
      1 + count(sub)                 # => 1, 1, 2, ...
    else
      0
    end
  end

  def path planet
    sub = planets[planet]

    if sub then
      [sub] + path(sub)
    else
      []
    end
  end

  def path_length from, to
    a = path from        # => ["J", "E", "D", "C", "B", "COM"]
    b = path to          # => ["D", "C", "B", "COM"]

    same = (a & b).first # => "D"

    d = count from       # => 6
    e = count to         # => 4
    f = count same       # => 3

    d + e - f - f        # => 4
  end
end

if ARGV.empty? then
  require "minitest/autorun"

  class Test06 < Minitest::Test
    INPUT = <<~EOM
      COM)B
      B)C
      C)D
      D)E
      E)F
      B)G
      G)H
      D)I
      E)J
      J)K
      K)L
    EOM

    def test_a
      assert_equal 42, Planets.new(INPUT).run_a
    end

    def test_b
      skip
    end
  end
else
  input = ARGF.read.chomp
  p Planets.new(input).run_a
  p Planets.new(input).run_b
end
