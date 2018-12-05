#!/usr/bin/env ruby -w

a = b = c = d = e = f = g = h = 0

a = 0

debug = ->(x) { p [x, a, b, c, d, e, f, g, h] }

b = 57                   #  0: set b 57
c = b                    #  1: set c b
if a.nonzero? then       #  2: jnz a 2
                         #  3: jnz 1 5
  b *= 100               #  4: mul b 100
  b += 100000            #  5: sub b -100000
  c = b                  #  6: set c b
  c += 17000             #  7: sub c -17000
end
loop do
  debug[:loop1]
  f = 1                  #  8: set f 1
  d = 2                  #  9: set d 2
  loop do
    debug[:loop2]
    e = 2                # 10: set e 2
    loop do
      debug[:loop3]
      g = d              # 11: set g d
      g *= e             # 12: mul g e
      g -= b             # 13: sub g b
      if g.zero? then    # 14: jnz g 2
        f = 0            # 15: set f 0
      end
      e += 1             # 16: sub e -1
      g = e              # 17: set g e
      g -= b             # 18: sub g b
      # debug[:g1]
      next if g.nonzero? # 19: jnz g -8
      break
    end
    d += 1               # 20: sub d -1
    g = d                # 21: set g d
    g -= b               # 22: sub g b
    # debug[:g2]
    next if g.nonzero?   # 23: jnz g -13
    break
  end
  if f.zero? then     # 24: jnz f 2
    h += 1               # 25: sub h -1
  end
  g = b                  # 26: set g b
  g -= c                 # 27: sub g c
  # debug[:g3]
  if g.zero? then        # 28: jnz g 2
    break                # 29: jnz 1 3
  else
    b += 17              # 30: sub b -17
    next                 # 31: jnz 1 -23
  end
end

debug[:final]
