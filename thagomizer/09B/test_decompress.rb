require 'minitest/autorun'
require './decompress.rb'

class CompressedStringTest < Minitest::Test
  def test_no_compression
    @str = CompressedString.new "ADVENT"

    assert_equal 6, @str.length
  end

  def test_one_decompression
    @str = CompressedString.new "A(1x5)BC"

    assert_equal 7, @str.length

    @str = CompressedString.new "(3x3)XYZ"

    assert_equal 9, @str.length
  end

  def test_two_decompressions
    @str = CompressedString.new "A(2x2)BCD(2x2)EFG"

    assert_equal 11, @str.length
  end

  def test_compressions_in_a_row
    @str = CompressedString.new "(6x1)(1x3)A"

    assert_equal 3, @str.length
  end

  def test_nested_compression
    @str = CompressedString.new "X(8x2)(3x3)ABCY"

    assert_equal 20, @str.length

    @str = CompressedString.new "(27x12)(20x12)(13x14)(7x10)(1x12)A"

    assert_equal 241920, @str.length

    @str = CompressedString.new "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"

    assert_equal 445, @str.length
  end
end
