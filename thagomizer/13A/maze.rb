class Maze

  def initialize rows, cols, wall_proc
    @wall_proc = wall_proc
    @rows = rows
    @cols = cols
    @maze = []

    @rows.times do |r|
      @maze[r] = []

      @cols.times do |c|
        @maze[r] << (@wall_proc.call(r, c) ? "#" : ".")
      end
    end
  end

  def self.new_from_x_y x, y
    Maze.new y, x
  end

  def neighbors r, c
    neighbors = []

    neighbors << [r, c - 1] if c > 0
    neighbors << [r, c + 1] if c < @cols - 1
    neighbors << [r - 1, c] if r > 0
    neighbors << [r + 1, c] if r < @rows - 1

    neighbors
  end

  def to_s
    @maze.map { |row| row.join("")}.join("\n")
  end

  def shortest_distance_rc from, to
    @maze[from[0]][from[1]] = 0

    stack = [from]

    until stack.empty? do
      current = stack.shift
      distance = @maze[current[0]][current[1]]

      return distance if current == to

      neighbors(*current).each do |n_r, n_c|
        case
        when @maze[n_r][n_c] == "#"
          next
        when @maze[n_r][n_c] == "."
          @maze[n_r][n_c] = distance + 1
          stack << [n_r, n_c]
        when @maze[n_r][n_c] > (distance + 1)
          @maze[n_r][n_c] = distance + 1
          stack << [n_r, n_c]
        end
      end
    end
  end

  def shortest_distance_xy from_xy, to_xy
    shortest_distance_rc(from_xy.reverse, to_xy.reverse)
  end
end
