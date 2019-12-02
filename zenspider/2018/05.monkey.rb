#!/usr/bin/env ruby -w

module Enumerable
  def foldr m=nil
    reverse.inject(m){ |acc,i| yield acc, i }
  end
end

class Alchemy
  def self.remove_polymers polymers
    polymers.chars.foldr("") do |ys,x|
      y = if !ys.empty? then ys.chars.first; end
      if y.nil?
        x.to_s
      elsif y != x && y.upcase == x.upcase
        ys[1..ys.length-1]
      else
        x + ys
      end
    end
  end

  def self.remove_specific_polymers polymers, specific
    remove_polymers polymers.gsub(specific.upcase, '').gsub(specific.downcase, '')
  end

  def self.find_shortest_polymers polymers
    ('a'..'z')
      .map { |char| remove_specific_polymers polymers, char }
      .min_by{ |r| r.length }
  end
end

if __FILE__ == $0
  s = ARGF.read.chomp
  puts Alchemy.remove_polymers(s.dup).length
  puts Alchemy.find_shortest_polymers(s.dup).length
end
