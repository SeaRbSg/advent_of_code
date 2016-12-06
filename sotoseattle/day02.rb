module Adventus

  DICT_D2 = [[1, 2, 3],
             [4, 5, 6],
             [7, 8, 9]]
  WALK_D2 = {"U" => [-1,0], "D" => [+1,0], "L" => [0,-1], "R" => [0,+1]}

  def bathroom_code instructions
    spot = [1,1]

    instructions.split("\n").map(&:chars).map do |set|
      set.each {|step| spot = add(spot, WALK_D2[step]) }
      DICT_D2[spot.first][spot.last]
    end.join
  end

  def add x, y
    a, b = [x.first + y.first, x.last + y.last]
    [crop(a), crop(b)]
  end

  def crop n
    return 0 if n < 0
    return 2 if n > 2
    n
  end

end
