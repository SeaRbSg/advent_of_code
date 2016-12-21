require 'pp'

class LogicPuzzle
  def self.safe? floors
    floors.each do |floor|
      groups = floor.group_by { |item, element| item }

      return true if groups.length <= 1

      groups[:mc].each do |_, element|
        unless groups[:g].any? { |_, e| e == element }
          return false
        end
      end
    end
  end

  def self.valid_move? state, move
    cargo, start_floor, end_floor = *move

    return false unless cargo.length == 1 or cargo.length == 2
    return false if end_floor > 3
    return false if end_floor < 0
    return false unless state[start_floor].include? [:e]

    cargo.each do |c|
      return false unless state[start_floor].include?(c)
    end

    true
  end

  def self.move state, move
    raise "Invalid!" unless valid_move?(state, move)

    cargo, start_floor, end_floor = *move

    cargo << [:e]

    cargo.each do |c|
      state[start_floor].delete c
      state[end_floor] << c
    end

    state
  end

  def initialize
    @cache       = {}
    @debug       = false
  end

  def solve start_state, goal_state, moves_made
    puts "STARTING"
    pp start_state
    pp goal_state

    if start_state == goal_state then
      puts "Moves Made #{moves_made.length}"
      pp moves_made if @debug
      return
    end

    elevator_at = start_state.index { |floor| floor.include? [:e] }

    # Items one by one
    move = nil
    start_state[elevator_at].each do |item|
      next if item == [:e]
      if elevator_at != 3
        move = [[item], elevator_at, elevator_at + 1]
puts "move"
pp move
        if LogicPuzzle.valid_move? start_state, move
          new_state = LogicPuzzle.move start_state.clone, move
          solve(new_state, goal_state, moves_made + [move])
        end
      end

#       if elevator_at != 0
#         move = [[item], elevator_at, elevator_at - 1]
# pp move
#         if LogicPuzzle.valid_move? start_state, move
# puts "Valid"
#           new_state = LogicPuzzle.move start_state.clone, move
#           solve new_state, goal_state, moves_made << move
#         end
#       end
    end
  end
end

lp = LogicPuzzle.new

start_state = [[], [], [[:e], [:mc, "A"], [:g, "A"]], []]
goal_state =  [[], [], [[:mc, "A"]], [[:g, "A"], [:e]]]

lp.solve start_state, goal_state, []
