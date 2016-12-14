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

    assert_equal 6, @str.length
  end

  def test_nested_compression
    @str = CompressedString.new "X(8x2)(3x3)ABCY"

    assert_equal 18, @str.length
  end
end
