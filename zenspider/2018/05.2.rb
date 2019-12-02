#!/usr/bin/env ruby -w

f = File.read("05.txt").chomp

AZ = "a".."z"

def time
  t0 = Time.now
  x = yield
  p Time.now - t0
  x
end

def clean input
  s1 = input.dup
  s2 = s1.dup

  loop do
    AZ.each do |c|
      cc = c.upcase
      s2.gsub!(/#{c}#{cc}|#{cc}#{c}/, "")
    end

    break if s1 == s2

    s1 = s2.dup
  end

  s2
end

p time { clean(f).size }
puts
p time { AZ.map { |c| clean(f.delete "#{c}#{c.upcase}").size }.min }
