require 'pp'

class LogicPuzzle
  attr_accessor :state

  FLOORS = [0, 1, 2, 3]

  def initialize filename
    @state = File.read(filename).split("\n").map(&:split)
    (0..3).each do |i|
      @state[i] ||= []
    end
  end

  def fries? item_a, item_b
    item_a[0] != item_b[0] and  # same element
      item_a[1] != item_b[1] # same type (chip or generator)
  end

  def safe_move? items, from, to
    return false unless items.length.between?(1, 2)
    return false unless @state[from].include? "E"
    return false unless items.all? { |item| @state[from].include?(item) }

    if items.length == 2
      return false if fries?(*items)
    end

    # Check each floor to see if any chips get fried
    (from+1..to).each do |floor|
      items.each do |item|
        return false if @state[floor].any? { |f_item| fries?(item, f_item) }
      end
    end

    true
  end

  def elevator_floor
    @state.find_index { |f| f.include? "E" }
  end

  def all_moves
    current_floor = elevator_floor
    floors = FLOORS - [current_floor]
    items = []

    (@state[current_floor] - ["E"]).combination(1) { |i| items << i }
    (@state[current_floor] - ["E"]).combination(2) { |i| items << i }

    moves = []
    items.each do |i|
      floors.each do |f|
        moves << [i.sort, current_floor, f]
      end
    end

    moves
  end

  def safe_moves
    all_moves.select { |m| safe_move? *m }
  end
end

# lp = LogicPuzzle.new "raw_data.txt"

# pp lp.state
