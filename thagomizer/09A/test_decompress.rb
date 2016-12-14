require 'minitest/autorun'
require './decompress.rb'

class CompressedStringTest < Minitest::Test
  def test_no_compression
    @str = CompressedString.new "ADVENT"
  end
end
