require 'minitest/autorun'
require './scrambler.rb'

class ScramblerTest < Minitest::Test
  def setup
    @s = Scrambler.new("abcde", [])
  end

  def test_swap_pos
    @s.instructions = ["swap position 4 with position 0"]

    @s.scramble

    assert_equal "ebcda", @s.str
  end

  def test_swap_letter
    @s.instructions = ["swap letter d with letter b"]

    @s.scramble

    assert_equal "adcbe", @s.str
  end

  def test_reverse_positions
    @s.instructions = ["reverse positions 0 through 4"]

    @s.scramble

    assert_equal "edcba", @s.str

    @s.instructions = ["reverse positions 1 through 3"]

    @s.scramble

    assert_equal "ebcda", @s.str
  end

  def test_rotate
    @s.instructions = ["rotate left 1 step"]

    @s.scramble

    assert_equal "bcdea", @s.str

    @s.instructions = ["rotate right 3 steps"]

    @s.scramble

    assert_equal "deabc", @s.str
  end

  def test_rotate_by_position
    @s.instructions = ["rotate based on position of letter c"]

    @s.scramble

    assert_equal "cdeab", @s.str

    @s.str = "abcdefghi"
    @s.instructions = ["rotate based on position of letter f"]

    @s.scramble

    assert_equal "cdefghiab", @s.str
  end

  def test_move_position
    @s.instructions = ["move position 1 to position 2"]

    @s.scramble

    assert_equal "acbde", @s.str

    @s.str = "bdeac"
    @s.instructions = ["move position 3 to position 0"]

    @s.scramble

    assert_equal "abdec", @s.str
  end

  def test_unscramble_swap_pos
    @s.str = "ebcda"
    @s.instructions = ["swap position 4 with position 0"]

    @s.unscramble

    assert_equal "abcde", @s.str
  end

  def test_unscramble_swap_letter
    @s.str = "edcba"
    @s.instructions = ["swap letter d with letter b"]

    @s.unscramble

    assert_equal "ebcda", @s.str
  end

  def test_reverse_positions
    @s.str = "abcde"
    @s.instructions = ["reverse positions 0 through 4"]

    @s.unscramble

    assert_equal "edcba", @s.str

    @s.str = "adcbe"
    @s.instructions = ["reverse positions 1 through 3"]

    @s.unscramble

    assert_equal "abcde", @s.str
  end

  def test_rotate
    @s.str = "bcdea"
    @s.instructions = ["rotate left 1 step"]

    @s.unscramble

    assert_equal "abcde", @s.str

    @s.str = "cdeab"
    @s.instructions = ["rotate right 3 steps"]

    @s.unscramble

    assert_equal "abcde", @s.str
  end

  def test_rotate_by_position
    @s.str = "ecabd"
    @s.instructions = ["rotate based on position of letter b"]

    @s.unscramble

    assert_equal "abdec", @s.str
  end

  def test_move_position
    @s.str = "abdec"
    @s.instructions = ["move position 3 to position 0"]

    @s.unscramble

    assert_equal "bdeac", @s.str

    @s.str = "bdeac"
    @s.instructions = ["move position 1 to position 4"]

    @s.unscramble

    assert_equal "bcdea", @s.str
  end
end
