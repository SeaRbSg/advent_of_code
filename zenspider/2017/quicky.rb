#!/usr/bin/ruby -w

require "pp"

class String
  def split23
    size  = Math.sqrt(self.size).to_i
    lines = chars.each_slice(size)
    side  = size.even? ? 2 : 3

    lines.each_slice(side).flat_map { |a, *rest|
      a.each_slice(side).zip(*rest.map { |e| e.each_slice(side) }).map(&:join)
    }
  end
end

pp "0123".split23                                 # 2x2
pp "012345678".split23                            # 3x3
pp "0123456789ABCDEF".split23                     # 4x4
pp "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".split23 # 6x6
pp "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".chars.cycle.take(81).join.split23
