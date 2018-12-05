#!/usr/bin/env ruby -w

require "prime"

a = b = c = d = e = f = g = h = 0

a = 1

b = 57
c = b

if a.nonzero? then
  b = b * 100 + 100_000
  c = b + 17_000
end

(b..c).step(17).each do |b|
  h += 1 unless b.prime?
end

debug[:final]
