class BlockNavigation
  TURN_MAPPING = {"L" => {:n => :w, :w => :s, :s => :e, :e => :n},
                  "R" => {:n => :e, :e => :s, :s => :w, :w => :n}}

  def initialize path
    @curr_heading  = :n
    @curr_location = [0, 0]
    @visited = []
    @directions = File.read(path).split(",").map(&:strip)
  end

  def move blocks
    while blocks > 0
      case @curr_heading
      when :n
        @curr_location[1] += 1
      when :s
        @curr_location[1] -= 1
      when :e
        @curr_location[0] += 1
      when :w
        @curr_location[0] -= 1
      end

      if @visited.include?(@curr_location)
        print_distance
        exit
      end

      @visited << [@curr_location[0], @curr_location[1]]

      blocks -= 1
    end
  end

  def print_distance
    puts @curr_location[0].abs + @curr_location[1].abs
  end

  def process_directions
    @directions.each do |direction|
      turn, num = /([LR])(\d+)/.match(direction).captures

      @curr_heading = TURN_MAPPING[turn][@curr_heading]

      move num.to_i
    end
  end
end

bl = BlockNavigation.new "data.txt"
bl.process_directions
