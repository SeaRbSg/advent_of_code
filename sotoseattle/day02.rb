module Adventus

  class Keypad
    DICT = {"U" => [-1,0], "D" => [+1,0], "L" => [0,-1], "R" => [0,+1]}
    Coord = Struct.new(:x, :y)

    def initialize
      @max = @pad.first.size - 1
    end

    def decypher instructions
      position = @start
      instructions.split.map(&:chars).map do |letters|
        letters.each { |c| position = decode(position, Coord.new(*DICT[c])) }
        translate position
      end.join
    end

    def decode pos, δ
      new_pos = Coord.new pos.x + δ.x, pos.y + δ.y
      new_pos.x = 0 if new_pos.x < 0
      new_pos.y = 0 if new_pos.y < 0
      new_pos.x = @max if new_pos.x > @max
      new_pos.y = @max if new_pos.y > @max
      return pos if translate(new_pos) == ""
      new_pos
    end

    def translate coords
      @pad[coords.x][coords.y]
    end
  end

  class Square_Keypad < Keypad
    attr_reader :pad, :start
    def initialize
      @pad = [["1", "2", "3"], ["4", "5", "6"], ["7", "8", "9"]]
      @start = Coord.new 1, 1
      super
    end
  end

  class Diamond_Keypad < Keypad
    attr_reader :pad, :start
    def initialize
      @pad = [["" , "" , "1", "" , "" ],
              ["" , "2", "3", "4", "" ],
              ["5", "6", "7", "8", "9"],
              ["" , "A", "B", "C", "" ],
              ["" , "" , "D", "" , "" ]]
      @start = Coord.new 2, 0
      super
    end
  end

end
