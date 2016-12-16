class Maze

  def initialize wall_proc
    @wall_proc = wall_proc
    @maze = Hash.new { |h, k| h[k] = [] }
  end

  def [] r, c
    @maze[r][c] ||= @wall_proc.call(r, c) ? "#" : "."
  end

  def []= r, c, v
    @maze[r][c] = v
  end

  def neighbors r, c
    neighbors = []

    neighbors << [r, c - 1] if c > 0
    neighbors << [r, c + 1]
    neighbors << [r - 1, c] if r > 0
    neighbors << [r + 1, c]

    neighbors
  end

  def to_s
    @maze.each_value.map { |row| row.join("")}.join("\n")
  end

  def breadth_first_search_distance start, &block
    self[*start] = 0

    stack = [start]

    until stack.empty? do
      current = stack.shift
      distance = self[*current]

      block.call current, distance

      neighbors(*current).each do |n|
        case
        when self[*n] == "#"
          next
        when self[*n] == "."
          self[*n] = distance + 1
          stack << n
        when self[*n] > (distance + 1)
          self[*n] = distance + 1
          stack << n
        end
      end
    end
  end

  def shortest_distance_rc from, to
    return nil if self[*to] == "#" or self[*from] == "#"

    breadth_first_search_distance [1, 1] do |location, distance|
      return distance if location == to
    end

    nil
  end

  def shortest_distance_xy from_xy, to_xy
    shortest_distance_rc(from_xy.reverse, to_xy.reverse)
  end

  def within_n_steps_of start, n
    in_range = [[1, 1]]

    breadth_first_search_distance [1, 1] do |location, distance|
      in_range << location if distance < n
    end

    in_range.length
  end
end
