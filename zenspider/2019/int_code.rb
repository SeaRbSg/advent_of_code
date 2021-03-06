class IntCode
  attr_accessor :mem
  attr_accessor :pos
  attr_accessor :input
  attr_accessor :output
  attr_accessor :done

  def initialize input
    self.mem     = input.chomp.split(/,/).map(&:to_i)
    self.pos     = 0
    self.input   = []
    self.output  = []
    self.done    = false
  end

  def arg1 ma
    a, = mem[pos+1, 1]

    n1 = a # HACK always position mode?

    self.pos += 2

    n1
  end

  def arg2 ma, mb
    a, b = mem[pos+1, 2]

    n1 = ma == "0" ? mem[a] : a
    n2 = mb == "0" ? mem[b] : b

    self.pos += 3

    [n1, n2]
  end

  def arg3 ma, mb, mc
    a, b, c = mem[pos+1, 3]

    n1 = ma == "0" ? mem[a] : a
    n2 = mb == "0" ? mem[b] : b
    n3 = c

    self.pos += 4

    [n1, n2, n3]
  end

  def step
    inst = "%05d" % mem[pos]

    mc, mb, ma, op = inst[0], inst[1], inst[2], inst[3..4]

    case op
    when "99" then # HALT
      self.done = true
    when "01" then # ADD
      n1, n2, n3 = arg3 ma, mb, mc

      mem[n3] = n1 + n2
    when "02" then # MUL
      n1, n2, n3 = arg3 ma, mb, mc

      mem[n3] = n1 * n2
    when "03" then # READ
      n1 = arg1 ma

      mem[n1] = input.shift
    when "04" then # WRITE
      n1 = arg1 ma

      output << mem[n1]
    when "05" then # JTRUE
      n1, n2 = arg2 ma, mb

      self.pos = n2 unless n1.zero?
    when "06" then # JFALSE
      n1, n2 = arg2 ma, mb

      self.pos = n2 if n1.zero?
    when "07" then # LT
      n1, n2, n3 = arg3 ma, mb, mc

      mem[n3] = n1 < n2 ? 1 : 0
    when "08" then # EQ
      n1, n2, n3 = arg3 ma, mb, mc

      mem[n3] = n1 == n2 ? 1 : 0
    else
      abort "BAD OP!: #{inst}"
    end
  end

  def run
    step until done
  end
end
