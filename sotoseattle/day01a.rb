module Adventus

  WHEEL = %w{N E S W N}
  COORD = {"N" => [1, 0], "S" => [-1, 0], "W" => [0, -1], "E" => [0, 1]}

  def distance_in_blocks directions
    tack = "N"
    spot = [0, 0]

    directions.split(", ")
              .map {|e| e.scan(/(R|L)(\d+)/).flatten}
              .each do |c, n|
                turn = (c == "R" ? WHEEL : WHEEL.reverse)
                tack = turn[turn.find_index(tack).next]
                jump = COORD[tack].map { |e| e * n.to_i }
                spot = spot.zip(jump).map{|e| e.reduce(:+)}
    end

    spot.map(&:abs).reduce(:+)
  end

end
