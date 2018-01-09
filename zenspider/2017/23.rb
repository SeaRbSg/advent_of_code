#!/usr/bin/env ruby -w

require "prime"

class String
  def to_v
    Integer(self) rescue self.to_sym
  end
end

class Problem23a
  attr_accessor :register, :program, :pc, :counter

  def initialize
    self.register = Hash.new { |h,k| raise "Unknown register: #{k}" }
    [:a, :b, :c, :d, :e, :f, :g, :h].each do |name|
      register[name] = 0
    end

    self.pc = 0
    self.counter = 0
  end

  def parse input
    self.program = input.lines.map { |line|
      case line
      when /set (\S+) (\S+)/ then # set X Y: sets register X to value Y
        x, y = $1.to_v, $2.to_v
        if y.is_a? Integer then
          ->() { register[x] = y;            step }
        else
          ->() { register[x] = register[y];  step }
        end
      when /sub (\S+) (\S+)/ then # sub X Y: subtracts register X by value Y
        x, y = $1.to_v, $2.to_v

        if y.is_a? Integer then
          ->() { register[x] -= y;           step }
        else
          ->() { register[x] -= register[y]; step }
        end
      when /mul (\S+) (\S+)/ then # mul X Y: multiplies register X by value Y
        x, y = $1.to_v, $2.to_v

        if y.is_a? Integer then
          ->() { register[x] *= y;           count; step }
        else
          ->() { register[x] *= register[y]; count; step }
        end
      when /jnz (\S+) (\S+)/ then # jnz X Y: pc += value Y, if the value X != 0
        x, y = $1.to_v, $2.to_v

        if x.is_a? Integer then
          if x != 0 then
            ->() { step y }
          else
            ->() { step 1 }
          end
        else
          if x.is_a? Integer then
            ->() { step (x.zero?           ? 1 : y) }
          else
            ->() { step (register[x].zero? ? 1 : y) }
          end
        end
      else
        warn "unparsed: #{line.chomp}"
      end
    }
  end

  def step n = 1
    self.pc += n
  end

  def count
    self.counter += 1
  end

  def run input
    parse input

    old_pc = self.pc
    inst = program[pc]
    begin
      inst.call
      old_pc = self.pc
    end while inst = program[old_pc]

    self
  end
end

class Problem23b < Problem23a
  def initialize
    super
    register[:a] = 1
  end

  def run
    b = 57 * 100 + 100_000
    c = b + 17_000
    h = 0

    (b..c).step(17).each do |bb|
      h += 1 unless bb.prime?
    end

    h
  end
end

if __FILE__ == $0 then
  if ARGV.empty? then
    require "minitest/autorun"

    class Test23 < Minitest::Test
      def test_set
        cpu = Problem23a.new

        assert_equal  0, cpu.register[:a]
        cpu.run "set a 42"
        assert_equal 42, cpu.register[:a]
        assert_equal 1, cpu.pc
      end

      def test_sub
        cpu = Problem23a.new

        assert_equal  0, cpu.register[:a]
        cpu.run "sub a -42"
        assert_equal 42, cpu.register[:a]
        assert_equal 1, cpu.pc
      end

      def test_mul
        cpu = Problem23a.new
        cpu.register[:a] = 2

        cpu.run "mul a 21"
        assert_equal 42, cpu.register[:a]
        assert_equal 1, cpu.pc
        assert_equal 1, cpu.counter
      end

      def test_jnz_true
        cpu = Problem23a.new
        cpu.register[:a] = 1

        cpu.run "jnz a 42"
        assert_equal 42, cpu.pc
      end

      def test_jnz_true_const
        cpu = Problem23a.new
        cpu.register[:a] = 1

        cpu.run "jnz 1 42"
        assert_equal 42, cpu.pc
      end

      def test_jnz_false
        cpu = Problem23a.new
        cpu.register[:a] = 0 # redundant

        cpu.run "jnz a 42"
        assert_equal 1, cpu.pc
      end

      def test_b
        skip
      end
    end
  else
    input = ARGF.read.chomp
    p Problem23a.new.run(input).counter
    p Problem23b.new.run
  end
end
