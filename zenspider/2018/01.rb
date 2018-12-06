#!/usr/bin/env ruby -w

nums = File.readlines("01.txt").map(&:to_i)

p nums.sum

seen = Hash.new 0

p nums.cycle.reduce(0) { |freq, n|
  m = freq + n
  break m if (seen[m] += 1) > 1
  m
}
