require 'minitest/autorun'
require './logic_puzzle.rb'

class LogicPuzzleTest < Minitest::Test
  def setup
    @puzzle = LogicPuzzle.new
  end

  def test_instantiation
    assert @puzzle
    assert_equal Array, @puzzle.floors.class
  end

  def test_safe_by_default
    assert @puzzle.safe?
  end

  def test_safe_microchips_with_microchips
    @puzzle.floors = [[[:mc, "A"], [:mc, "B"]], [[:mc, "X"], [:mc, "Y"]]]
    assert @puzzle.safe?
  end

  def test_safe_generators_with_generators
    @puzzle.floors = [[[:g, "A"], [:g, "B"]], [[:g, "X"], [:g, "Y"]]]
    assert @puzzle.safe?
  end

  def test_unsafe_chips_and_generators_mixed_type
    @puzzle.floors =[[[:g, "A"], [:mc, "B"]], [[:g, "B"], [:mc, "A"]]]
    refute @puzzle.safe?
  end

  def test_invalid_move_no_cargo
    @puzzle.floors = [[[:g, "A"]], [], [], []]

    # Move nothing up one floor, start at floor 1
    refute @puzzle.allowed_move? [], :up, 1
  end

  def test_valid_move
    @puzzle.floors = [[[:g, "A"]], [], [], []]

    assert @puzzle.allowed_move? [[:g, "A"]], :up, 0
  end

  def test_invalid_move_cargo_not_there
    @puzzle.floors = [[[:g, "A"]], [], [], []]

    refute @puzzle.allowed_move? [[:g, "B"]], :up, 0
  end

  def test_invalid_move_building_bounds
    @puzzle.floors = [[[:g, "A"]], [], [], []]

    refute @puzzle.allowed_move? [[:g, "A"]], :down, 0

    @puzzle.elevator = 3
    refute @puzzle.allowed_move? [[:g, "A"]], :up, 3
  end

  def test_invalid_move_elevator_on_wrong_floor
    @puzzle.floors = [[[:g, "A"]], [], [], []]
    @puzzle.elevator = 1

    refute @puzzle.allowed_move? [[:g, "A"]], :up, 0
  end

  def test_move
    @puzzle.floors = [[[:g, "A"]], [], [], []]

    @puzzle.move [[:g, "A"]], :up, 0

    assert_equal [[], [[:g, "A"]], [], []], @puzzle.floors
  end
end
