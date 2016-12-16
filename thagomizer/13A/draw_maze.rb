require 'pp'
require './maze.rb'

seed = ARGV[0].to_i
rows = ARGV[1].to_i
cols = ARGV[2].to_i
find_x = ARGV[3].to_i
find_y = ARGV[4].to_i

wall_proc = lambda { |r, c| (c*c + 3*c + 2*c*r + r + r*r + seed).to_s(2).each_char.select {|x| x == "1"}.length % 2 == 1 }

@maze = Maze.new rows, cols, wall_proc

puts @maze.shortest_distance_xy [1, 1], [find_x, find_y]
