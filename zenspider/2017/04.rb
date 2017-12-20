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

    class Test04 < Minitest::Test
      def assert_valid input
        assert_operator Problem04a.new, :valid?, input
      end

      def refute_valid input
        refute_operator Problem04a.new, :valid?, input
      end

      def test_a_valid
        assert_valid "aa bb cc dd ee"
        assert_valid "aa bb cc dd aaa"
        refute_valid "aa bb cc dd aa"
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
