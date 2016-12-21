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
end
