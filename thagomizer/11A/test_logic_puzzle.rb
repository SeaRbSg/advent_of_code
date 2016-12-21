require 'minitest/autorun'
require './logic_puzzle.rb'

class LogicPuzzleTest < Minitest::Test
  def test_empty_building_safe
    LogicPuzzle.safe? [[], [], [], []]
  end

  def test_safe_microchips_with_microchips
    assert LogicPuzzle.safe? [[[:mc, "A"], [:mc, "B"]], [[:mc, "X"], [:mc, "Y"]]]
  end

  def test_safe_generators_with_generators
    assert LogicPuzzle.safe? [[[:g, "A"], [:g, "B"]], [[:g, "X"], [:g, "Y"]]]
  end

  def test_unsafe_chips_and_generators_mixed_type
    refute LogicPuzzle.safe? [[[:g, "A"], [:mc, "B"]], [[:g, "B"], [:mc, "A"]]]
  end

  def test_invalid_move_no_cargo
    state = [[[:g, "A"]], [], [], []]
    move  = [[], 0, 1]                  # Move nothing from floor 0 to floor 1

    refute LogicPuzzle.valid_move? state, move
  end

  def test_valid_move
    state = [[[:g, "A"], [:e]], [], [], []]
    move  = [[[:g, "A"]], 0, 1]

    assert LogicPuzzle.valid_move? state, move
  end

  def test_invalid_move_cargo_not_there
    move = [[[:g, "B"]], 0, 1]
    state = [[[:g, "A"], [:e]], [], [], []]

    refute LogicPuzzle.valid_move? state, move
  end

  def test_invalid_move_building_bounds
    state = [[[:g, "A"]], [], [], []]
    move  = [[[:g, "A"]], 0, -1]

    refute LogicPuzzle.valid_move? state, move

    move = [[[:g, "A"]], 3, 4]
    refute LogicPuzzle.valid_move? state, move
  end

  def test_invalid_move_elevator_on_wrong_floor
    state = [[[:g, "A"]], [[:e]], [], []]
    move = [[[:g, "A"]], 0, 1]

    refute LogicPuzzle.valid_move? state, move
  end

  def test_move
    start_state = [[[:g, "A"], [:e]], [], [], []]
    move = [[[:g, "A"]], 0, 1]

    end_state = [[], [[:g, "A"], [:e]], [], []]

    assert_equal end_state, LogicPuzzle.move(start_state, move)
  end
end
