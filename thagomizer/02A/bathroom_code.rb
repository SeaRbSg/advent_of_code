class BathroomCode
  attr_accessor :code

  KEYPAD = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

  def initialize path
    @code = ""
    @curr_pos = [1, 1]
    @puzzle = File.readlines(path).map{ |l| l.strip.split("") }
  end

  def process_puzzle
    @puzzle.each do |line|
      line.each do |move|
        case move
        when "U"
          @curr_pos[0] -= 1 unless @curr_pos[0] == 0
        when "D"
          @curr_pos[0] += 1 unless @curr_pos[0] == 2
        when "L"
          @curr_pos[1] -= 1 unless @curr_pos[1] == 0
        when "R"
          @curr_pos[1] += 1 unless @curr_pos[1] == 2
        end
      end

      @code << KEYPAD[@curr_pos[0]][@curr_pos[1]].to_s
    end
  end
end

b = BathroomCode.new "data.txt"
b.process_puzzle

puts b.code
