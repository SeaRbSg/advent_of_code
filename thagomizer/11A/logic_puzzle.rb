require 'pp'

class LogicPuzzle
  attr_accessor :floors, :elevator

  def initialize
    @floors = [[], [], [], []]
    @elevator = 0
  end

  def safe?
    @floors.each do |floor|
      groups = floor.group_by { |item, element| item }

      return true if groups.length <= 1

      groups[:mc].each do |_, element|
        unless groups[:g].any? { |_, e| e == element }
          return false
        end
      end
    end
  end

  def allowed_move? cargo, direction, starting_floor
    return false unless cargo.length == 1 or cargo.length == 2
    return false unless @elevator == starting_floor
    return false if starting_floor == 0 and direction == :down
    return false if starting_floor == 3 and direction == :up

    cargo.each do |c|
      return false unless @floors[starting_floor].include?(c)
    end

    true
  end

  def move cargo, direction, starting_floor
    raise "Invalid!" unless allowed_move?(cargo, direction, starting_floor)

    new_floor = direction == :up ? starting_floor + 1 : starting_floor - 1

    cargo.each do |c|
      @floors[starting_floor].delete c
      @floors[new_floor] << c
    end

    @elevator = new_floor
  end
end
