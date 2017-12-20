#!/usr/bin/env ruby -w

class Problem03a
  def run n
    return 0 if n < 2

    ring(n) + off(n)
  end

  def ring n
    ((Math.sqrt(n) + 1) / 2).ceil - 1
  end

  def off n
    ring = self.ring n
    min  = (2*(ring-1) + 1) ** 2 + 1
    side = 2 * ring

    ((n - min) % side) - (ring-1)
  end
end

class Problem03b < Problem03a
  AROUND = [[ 0,  1], [-1,  1], [-1,  0], [-1, -1],
            [ 0, -1], [ 1, -1], [ 1,  0], [ 1,  1]]

  def spiral n_rings
    x, y, n = 0, 0, 1

    map = { [x, y] => n }

    n += 1
    x += 1

    n_rings.times.each do |ring|
      len = 2 * (ring+2) - 1
      # N            W           S            E
      [[0, 1, -2], [-1, 0, -1], [0, -1, -1], [1, 0, 0]].each do |dx, dy, dlen|
        (len+dlen).times do
          map[[x, y]] = yield map, x, y, n
          x += dx
          y += dy
          n += 1
        end
      end
    end

    map
  end

  def run m
    n_rings = ring m

    data = spiral(n_rings) { |map, x, y, n|
      x = AROUND.map { |(dx, dy)| map[[x+dx, y+dy]] || 0 }.sum
      break map if x > 2*m # TODO: this is a cheat
      x
    }

    data.values.sort.find { |n| n > m }
  end
end

if __FILE__ == $0 then
  if ARGV.empty? then
    require "minitest/autorun"

    class Test03 < Minitest::Test
      attr_accessor :o
      def setup
        @o = Problem03a.new
      end

      def test_ring
        assert_equal 0, o.ring( 1),  1
        assert_equal 1, o.ring( 2),  2
        assert_equal 1, o.ring( 9),  9
        assert_equal 2, o.ring(10), 10
        assert_equal 2, o.ring(25), 25
      end

      def test_a
        assert_equal 0, Problem03a.new.run(1), 1
        assert_equal 3, Problem03a.new.run(12), 12
        assert_equal 2, Problem03a.new.run(23), 23
        assert_equal 31, Problem03a.new.run(1024), 1024
      end

      def test_b
        assert_equal   4, Problem03b.new.run( 2), 2
        assert_equal   4, Problem03b.new.run( 3), 3
        assert_equal   5, Problem03b.new.run( 4), 4
        assert_equal  10, Problem03b.new.run( 5), 5
        assert_equal  10, Problem03b.new.run( 9), 9
        assert_equal  23, Problem03b.new.run(12), 12
        assert_equal  26, Problem03b.new.run(25), 23
      end
    end
  else
    input = Integer(ARGV.shift) rescue 265149
    p Problem03a.new.run input
    p Problem03b.new.run input
  end
end
