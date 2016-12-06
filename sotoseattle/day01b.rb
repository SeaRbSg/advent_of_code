module Adventus

  def distance_to_twice_visited_spot directions
    tack = "N"
    spot = [0, 0]
    path = []

    directions.split(", ")
              .map {|e| e.scan(/(R|L)(\d+)/).flatten}
              .each do |c, n|
                turn = (c == "R" ? WHEEL : WHEEL.reverse)
                tack = turn[turn.find_index(tack).next]
                n.to_i.times do
                  a, b, c, d = spot + COORD[tack]
                  spot = [a + c, b + d]

                  if path.include?(spot)
                    return distance(spot)
                  else
                    path << spot
                  end
                end
    end

    distance spot
  end

end
