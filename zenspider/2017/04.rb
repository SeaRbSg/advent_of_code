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
  def anagram? words
    not uniq? words.map { |word| word.chars.to_a.sort.join }
  end

  def valid? input
    not anagram? self.words input
  end
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
    end

    class Test04b < Test04a
      def setup
        self.o = Problem04b.new
      end

      def assert_anagram input
        assert_operator o, :anagram?, o.words(input)
      end

      def refute_anagram input
        refute_operator o, :anagram?, o.words(input)
      end

      def test_b_anagram
        refute_anagram "abcde fghij"
        assert_anagram "abcde xyz ecdab"
        refute_anagram "a ab abc abd abf abj"
        refute_anagram "iiii oiii ooii oooi oooo"
        assert_anagram "oiii ioii iioi iiio"
      end
    end
  else
    path = ARGV.shift
    p Problem04a.new.run path
    p Problem04b.new.run path
  end
end
