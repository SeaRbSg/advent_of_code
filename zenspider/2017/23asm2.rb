#!/usr/bin/env ruby -w

a = b = c = d = e = f = g = h = 0

a = 1

# debug = ->(x) { p [x, a, b, c, d, e, f, g, h] }
debug = ->(x) { }

b = 57                   #  0: set b 57
c = b                    #  1: set c b

if a.nonzero? then       #  2: jnz a 2
                         #  3: jnz 1 5
                         #  4: mul b 100
                         #  5: sub b -100000
  b = b * 100 + 100_000
                         #  6: set c b
                         #  7: sub c -17000
  c = b + 17_000
end

loop do
  debug[:loop1]
  f = 1                  #  8: set f 1
  d = 2                  #  9: set d 2

  (2..b).each do |dd|
    d = dd
    debug[:loop2]

    (2..b).each do |ee|
      e = ee # hack
      debug[:loop3]
      g = d * e - b
      f = 0 if g.zero?
      g = e + 1 - b
      break if g.zero?
    end
    e += 1 # hack

    g = d + 1 - b

    break if g.zero?
  end
  d += 1 # hack

  if f.zero? then        # 24: jnz f 2
    h += 1               # 25: sub h -1
  end
                         # 26: set g b
                         # 27: sub g c
  g = b - c

  if g.zero? then        # 28: jnz g 2
    break                # 29: jnz 1 3
  else
    b += 17              # 30: sub b -17
    next                 # 31: jnz 1 -23
  end
end

debug[:final]
