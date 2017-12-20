#!/usr/bin/env ruby -w

class Problem04a
  def run path
    File.readlines(path).count { |line| valid? line }
  end

  def words input
    input.split(/\s+/)
  end

  def uniq? words
    words.count == words.uniq.count
  end

  def valid? input
    uniq? self.words input
  end
end

class Problem04b < Problem04a
end

if __FILE__ == $0 then
  if ARGV.empty? then
    require "minitest/autorun"

    class Test04a < Minitest::Test
      attr_accessor :o

      def setup
        self.o = Problem04a.new
      end

      def assert_uniq input
        assert_operator o, :uniq?, o.words(input)
      end

      def refute_uniq input
        refute_operator o, :uniq?, o.words(input)
      end

      def test_a_words
        assert_equal %w[aa bb cc dd ee], o.words("aa bb cc dd ee")
      end

      def test_a_uniq
        assert_uniq "aa bb cc dd ee"
        assert_uniq "aa bb cc dd aaa"
        refute_uniq "aa bb cc dd aa"
      end

      def test_b
        skip
      end
    end
  else
    path = ARGV.shift
    p Problem04a.new.run path
    p Problem04b.new.run path
  end
end
