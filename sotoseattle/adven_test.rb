require 'minitest/autorun'
require 'minitest/pride'

require_relative './day01.rb'
require_relative './day02.rb'
require_relative './day03.rb'

class TestAdventus < Minitest::Test
  include Adventus

  def test_day01
    long_input = File.read("./test_files/input_day1.txt")

    assert_equal  2,  distance_in_blocks("R2, R2, R2")
    assert_equal  5,  distance_in_blocks("R2, L3")
    assert_equal 12,  distance_in_blocks("R5, L5, R5, R3")
    assert_equal 146, distance_in_blocks(long_input)

    assert_equal  4,  distance_to_twice_visited_spot("R8, R4, R4, R8")
    assert_equal 131, distance_to_twice_visited_spot(long_input)
   end

  def test_day02
    long_input = File.read("./test_files/input_day2.txt")

    assert_equal "1985",  Square_Keypad.new.decypher("ULL\nRRDDD\nLURDL\nUUUUD")
    assert_equal "78985", Square_Keypad.new.decypher(long_input)

    assert_equal "5DB3",  Diamond_Keypad.new.decypher("ULL\nRRDDD\nLURDL\nUUUUD")
    assert_equal "57DD8", Diamond_Keypad.new.decypher(long_input)
  end

  def test_day03
    long_input = File.read("./test_files/input_day3.txt")

    assert_equal 0, valid_triangles_by_row("  25  5  10 \n  5  25  10 \n  10  25  5 ")

    assert_equal 993,  valid_triangles_by_row(long_input)
    assert_equal 1849, valid_triangles_by_col(long_input)
  end

end
