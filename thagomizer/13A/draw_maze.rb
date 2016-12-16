require 'pp'

seed = ARGV[0].to_i
rows = ARGV[1].to_i
cols = ARGV[2].to_i

maze = []

rows.times do
  maze << Array.new(cols, ".")
end

maze.each_with_index do |row, r|
  row.each_with_index do |_, c|
    t = c*c + 3*c + 2*c*r + r + r*r + seed
    if t.to_s(2).split('').map(&:to_i).inject(&:+) % 2 == 1 then
      maze[r][c] = "#"
    end
  end
end

maze.each do |row|
  puts row.join("")
end

def find_neigbors r, c
  neighbors = []

  neighbors << [r, c - 1] if c > 0
  neighbors << [r, c + 1] if c < 80 - 1
  neighbors << [r - 1, c] if r > 0
  neighbors << [r + 1, c] if r < 50 - 1

  neighbors
end


final = [39, 31]
maze[1][1] = 0
stack = [[1, 1]] # starting position plus distance

i = 0
until stack.empty? do
  curr_r, curr_c = stack.shift
  distance = maze[curr_r][curr_c]

  if [curr_r, curr_c] == final
    puts distance
    exit
  end

  neighbors = find_neigbors(curr_r, curr_c)

  neighbors.each do |n_r, n_c|
    case
    when maze[n_r][n_c] == "#"
      next
    when maze[n_r][n_c] == "."
      maze[n_r][n_c] = distance + 1
      stack << [n_r, n_c]
    when maze[n_r][n_c] > (distance + 1)
      maze[n_r][n_c] = distance + 1
      stack << [n_r, n_c]
    end
  end

end
