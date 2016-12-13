require 'minitest/autorun'
require './screen.rb'

class ScreenTest < Minitest::Test
  def setup
    @screen = Screen.new(4, 4)
  end

  def test_to_s
    assert_equal "....\n....\n....\n....", @screen.to_s
  end


  def test_rect
    @screen.rect 2, 3

    assert_equal "##..\n##..\n##..\n....", @screen.to_s
  end

  def test_rotate_row
    @screen.rect 3, 2
    @screen.rotate_row 1, 3

    assert_equal "###.\n##.#\n....\n....", @screen.to_s
  end

  def test_rotate_column
    @screen.rect 3, 2
    @screen.rotate_column 2, 3

    assert_equal "###.\n##..\n....\n..#.", @screen.to_s
  end
end
