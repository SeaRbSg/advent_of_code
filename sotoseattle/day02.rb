module Adventus

  class Keypad_Square
    attr_reader :pad, :start, :max
    def initialize
      @pad = [["1", "2", "3"], ["4", "5", "6"], ["7", "8", "9"]]
      @start = [1, 1]
      @max = 2
    end
  end

  class Keypad_Diamond
    attr_reader :pad, :start, :max
    def initialize
      @pad = [["" , "" , "1", "" , "" ],
              ["" , "2", "3", "4", "" ],
              ["5", "6", "7", "8", "9"],
              ["" , "A", "B", "C", "" ],
              ["" , "" , "D", "" , "" ]]
      @start = [2, 0]
      @max = 4
    end
  end

  class Bathroom_Door
    attr_reader :keypad
    WALK = {"U" => [-1,0], "D" => [+1,0], "L" => [0,-1], "R" => [0,+1]}

    def initialize keypad
      @keypad = keypad.new
    end

    def decypher instructions
      spot = keypad.start

      instructions.split("\n").map(&:chars).map do |set|
        set.each { |step| spot = add(spot, WALK[step]) }
        translate spot
      end.join
    end

    def translate coords
      keypad.pad[coords.first][coords.last]
    end

    def add x, y
      a, b = [x.first + y.first, x.last + y.last]
      new_pos = [jump(a), jump(b)]
      return x if translate(new_pos) == ""
      new_pos
    end

    def jump n
      return 0 if n < 0
      return keypad.max if n > keypad.max
      n
    end
  end

  def bathroom_code_square instructions
    Bathroom_Door.new(Keypad_Square).decypher instructions
  end

  def bathroom_code_diamond instructions
    Bathroom_Door.new(Keypad_Diamond).decypher instructions
  end
end
