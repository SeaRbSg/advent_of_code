require 'minitest/autorun'
require './lib/logic_puzzle_2.rb'
require 'pp'

class TestLogicPuzzle < Minitest::Test
  def setup
    @puzzle = LogicPuzzle.new "test-data.txt"
  end

  def test_fries
    assert @puzzle.fries? "AM", "BG"

    refute @puzzle.fries? "AM", "AG"

    refute @puzzle.fries? "AM", "BM"

    refute @puzzle.fries? "AG", "BG"
  end

  def test_safe_move_too_many_items
    @puzzle.state = [["AG", "BG", "CG"],[],[],[]]

    refute @puzzle.safe_move? ["AG", "BG", "CG", "E"], 0, 3
  end

  def test_safe_move_too_few_items
    refute @puzzle.safe_move? [], 0, 3
  end

  def test_safe_move_item_not_on_starting_floor
    @puzzle.state = [["AG", "E"], ["BG"], [], []]

    refute @puzzle.safe_move? ["BG"], 0, 3

    assert @puzzle.safe_move? ["AG"], 0, 1
  end

  def test_safe_move_item_combinations
    @puzzle.state = [["AG", "BG", "AM", "BM", "E"], [], [], []]

    refute @puzzle.safe_move? ["AG", "BM"], 0, 3

    assert @puzzle.safe_move? ["AG", "AM"], 0, 3

    assert @puzzle.safe_move? ["AG", "BG"], 0, 3

    assert @puzzle.safe_move? ["AM", "BM"], 0, 3
  end

  def test_safe_move_elevator_must_be_at_start
    @puzzle.state = [["E", "AM"], [], [], []]

    refute @puzzle.safe_move? ["AM"], 1, 3

    assert @puzzle.safe_move? ["AM"], 0, 2
  end

  def test_safe_move_each_step_must_be_safe
    @puzzle.state = [["E", "AM"], ["BM"], ["BG"]]

    assert @puzzle.safe_move? ["AM"], 0, 1

    refute @puzzle.safe_move? ["AM"], 0, 2
  end

  def test_elevator_floor
    @puzzle.state = [["E"], [], [], []]

    assert_equal 0, @puzzle.elevator_floor

    @puzzle.state = [[], [], ["E"], []]

    assert_equal 2, @puzzle.elevator_floor
  end

  def test_all_moves
    @puzzle.state = [["E", "AM"], [], [], []]

    expected = [[["AM"], 0, 1], [["AM"], 0, 2,], [["AM"], 0, 3]]

    assert_equal expected, @puzzle.all_moves.sort
  end

  def test_all_moves_several_items
    @puzzle.state = [["E", "AM", "AG", "BM"], [], [], []]

    expected = [[["AG"], 0, 1], [["AG"], 0, 2,], [["AG"], 0, 3],
                [["AG", "AM"], 0, 1], [["AG", "AM"], 0, 2],
                [["AG", "AM"], 0, 3], [["AG", "BM"], 0, 1],
                [["AG", "BM"], 0, 2], [["AG", "BM"], 0, 3],
                [["AM"], 0, 1], [["AM"], 0, 2,], [["AM"], 0, 3],
                [["AM", "BM"], 0, 1], [["AM", "BM"], 0, 2],
                [["AM", "BM"], 0, 3],
                [["BM"], 0, 1], [["BM"], 0, 2,], [["BM"], 0, 3]
               ]

    actual = @puzzle.all_moves.sort

    assert_equal expected, actual
  end

  def test_valid_moves_several_items
    @puzzle.state = [["E", "AM", "AG", "BM"], [], [], []]

    expected = [[["AG"], 0, 1], [["AG"], 0, 2,], [["AG"], 0, 3],
                [["AG", "AM"], 0, 1], [["AG", "AM"], 0, 2],
                [["AG", "AM"], 0, 3],
                [["AM"], 0, 1], [["AM"], 0, 2,], [["AM"], 0, 3],
                [["AM", "BM"], 0, 1], [["AM", "BM"], 0, 2],
                [["AM", "BM"], 0, 3],
                [["BM"], 0, 1], [["BM"], 0, 2,], [["BM"], 0, 3]
               ]

    actual = @puzzle.safe_moves.sort

    assert_equal expected.length, actual.length
    assert_equal expected, actual
  end
end
