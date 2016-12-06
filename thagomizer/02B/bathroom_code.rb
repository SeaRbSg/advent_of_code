require "pp"

class BathroomCode
  attr_accessor :code

  KEYPAD = [[nil, nil,   1, nil, nil],
            [nil,   2,   3,   4, nil],
            [5,     6,   7,   8,   9],
            [nil, "A", "B", "C", nil],
            [nil, nil, "D", nil, nil]]
  ROW_BOUNDS = [0, 4]
  COL_BOUNDS = [0, 4]

  def initialize path
    @code = ""
    @curr_pos = [2, 0]
    @puzzle = File.readlines(path).map{ |l| l.strip.split("") }
  end

  def get_key pos
    KEYPAD[pos[0]][pos[1]]
  end

  def new_pos direction
    new_pos = @curr_pos.clone
    case direction
    when "U"
      new_pos[0] -= 1
    when "D"
      new_pos[0] += 1
    when "L"
      new_pos[1] -= 1
    when "R"
      new_pos[1] += 1
    end

    if new_pos[0].between?(*ROW_BOUNDS) and
       new_pos[1].between?(*COL_BOUNDS) and
       get_key(new_pos) then
      new_pos
    else
      @curr_pos
    end
  end

  def process_puzzle
    @puzzle.each do |line|
      line.each do |move|
        @curr_pos = new_pos move
      end

      @code << KEYPAD[@curr_pos[0]][@curr_pos[1]].to_s
    end
  end
end

b = BathroomCode.new "data.txt"
b.process_puzzle

puts b.code
