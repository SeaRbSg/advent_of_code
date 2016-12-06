directions = File.read("data.txt").split(",").map(&:strip)

curr_heading  = :n
curr_location = [0, 0]

turn_mapping = {"L" => {:n => :w, :w => :s, :s => :e, :e => :n},
                "R" => {:n => :e, :e => :s, :s => :w, :w => :n}}

directions.each do |direction|
  turn, num = /([LR])(\d+)/.match(direction).captures

  curr_heading = turn_mapping[turn][curr_heading]

  num = num.to_i

  case curr_heading
  when :n
    curr_location[1] += num
  when :s
    curr_location[1] -= num
  when :e
    curr_location[0] += num
  when :w
    curr_location[0] -= num
  end

end

puts curr_location[0].abs + curr_location[1].abs
