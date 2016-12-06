require 'minitest/autorun'
require 'minitest/pride'

require_relative './day01.rb'
require_relative './day02.rb'

class TestAdventus < Minitest::Test
  include Adventus

  def test_day01a
    assert_equal  2, distance_in_blocks("R2, R2, R2")
    assert_equal  5, distance_in_blocks("R2, L3")
    assert_equal 12, distance_in_blocks("R5, L5, R5, R3")

    assert_equal 146, distance_in_blocks("R4, R4, L1, R3, L5, R2, R5, R1, L4, \
                                          R3, L5, R2, L3, L4, L3, R1, R5, R1, \
                                          L3, L1, R3, L1, R2, R2, L2, R5, L3, \
                                          L4, R4, R4, R2, L4, L1, R5, L1, L4, \
                                          R4, L1, R1, L2, R5, L2, L3, R2, R1, \
                                          L194, R2, L4, R49, R1, R3, L5, L4,  \
                                          L1, R4, R2, R1, L5, R3, L5, L4, R4, \
                                          R4, L2, L3, R78, L5, R4, R191, R4,  \
                                          R3, R1, L2, R1, R3, L1, R3, R4, R2, \
                                          L2, R1, R4, L5, R2, L2, L4, L2, R1, \
                                          R2, L3, R5, R2, L3, L3, R3, L1, L1, \
                                          R5, L4, L4, L2, R5, R1, R4, L3, L5, \
                                          L4, R5, L4, R5, R4, L3, L2, L5, R4, \
                                          R3, L3, R1, L5, R5, R1, L3, R2, L5, \
                                          R5, L3, R1, R4, L5, R4, R2, R3, L4, \
                                          L5, R3, R4, L5, L5, R4, L4, L4, R1, \
                                          R5, R3, L1, L4, L3, L4, R1, L5, L1, \
                                          R2, R2, R4, R4, L5, R4, R1, L1, L1, \
                                          L3, L5, L2, R4, L3, L5, L4, L1, R3")
  end

  def test_day01b
    assert_equal  4, distance_to_twice_visited_spot("R8, R4, R4, R8")
    assert_equal 131, distance_to_twice_visited_spot(
                                         "R4, R4, L1, R3, L5, R2, R5, R1, L4, \
                                          R3, L5, R2, L3, L4, L3, R1, R5, R1, \
                                          L3, L1, R3, L1, R2, R2, L2, R5, L3, \
                                          L4, R4, R4, R2, L4, L1, R5, L1, L4, \
                                          R4, L1, R1, L2, R5, L2, L3, R2, R1, \
                                          L194, R2, L4, R49, R1, R3, L5, L4,  \
                                          L1, R4, R2, R1, L5, R3, L5, L4, R4, \
                                          R4, L2, L3, R78, L5, R4, R191, R4,  \
                                          R3, R1, L2, R1, R3, L1, R3, R4, R2, \
                                          L2, R1, R4, L5, R2, L2, L4, L2, R1, \
                                          R2, L3, R5, R2, L3, L3, R3, L1, L1, \
                                          R5, L4, L4, L2, R5, R1, R4, L3, L5, \
                                          L4, R5, L4, R5, R4, L3, L2, L5, R4, \
                                          R3, L3, R1, L5, R5, R1, L3, R2, L5, \
                                          R5, L3, R1, R4, L5, R4, R2, R3, L4, \
                                          L5, R3, R4, L5, L5, R4, L4, L4, R1, \
                                          R5, R3, L1, L4, L3, L4, R1, L5, L1, \
                                          R2, R2, R4, R4, L5, R4, R1, L1, L1, \
                                          L3, L5, L2, R4, L3, L5, L4, L1, R3")
   end

  def test_day02
    long_input = File.read("./test_files/input_day2.txt")

    assert_equal "1985",  Square_Keypad.new.decypher("ULL\nRRDDD\nLURDL\nUUUUD")
    assert_equal "78985", Square_Keypad.new.decypher(long_input)

    assert_equal "5DB3",  Diamond_Keypad.new.decypher("ULL\nRRDDD\nLURDL\nUUUUD")
    assert_equal "57DD8", Diamond_Keypad.new.decypher(long_input)
  end

end
