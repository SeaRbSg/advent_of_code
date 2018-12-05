#!/usr/bin/env ruby -w

f = File.read("05.txt").chomp

AZ = "a".."z"

def clean input
  s1 = input

  loop do
    s2 = AZ.reduce s1 do |s, c|
      cc = c.upcase
      s.gsub(/#{c}#{cc}|#{cc}#{c}/, "")
    end

    break if s1 == s2

    s1 = s2
  end

  s1
end

p clean(f).size
p AZ.map { |c| clean(f.delete "#{c}#{c.upcase}").size }.min
