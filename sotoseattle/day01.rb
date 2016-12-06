module Adventus

  WHEEL_D1 = %w{N E S W N}
  COORD_D1 = {"N" => [1, 0], "S" => [-1, 0], "W" => [0, -1], "E" => [0, 1]}

  def parse_directions dirs
    dirs.split(", ").map {|e| e.scan(/(R|L)(\d+)/).flatten}
  end

  def distance arr
    arr.map(&:abs).reduce(:+)
  end

  def initial_state
    ["N", [0, 0], []]
  end

  def turn original, dir
    arr = (dir == "R" ? WHEEL_D1 : WHEEL_D1.reverse)
    arr[arr.find_index(original).next]
  end

  def distance_in_blocks input
    tack, spot  = initial_state

    parse_directions(input).each do |dir, n|
      tack = turn tack, dir
      jump = COORD_D1[tack].map { |e| e * n.to_i }
      spot = spot.zip(jump).map{|e| e.reduce(:+)}
    end

    distance spot
  end

  def distance_to_twice_visited_spot input
    tack, spot, path = initial_state

    parse_directions(input).each do |dir, n|
      tack = turn tack, dir
      n.to_i.times do
        a, b, c, d = spot + COORD_D1[tack]
        spot = [a + c, b + d]

        return distance(spot) if path.include?(spot)
        path << spot
      end
    end

    distance spot
  end

end
