require 'pp'
require './maze.rb'

seed = ARGV[0].to_i
n    = ARGV[1].to_i

wall_proc = lambda { |r, c| (c*c + 3*c + 2*c*r + r + r*r + seed).to_s(2).each_char.select {|x| x == "1"}.length % 2 == 1 }

@maze = Maze.new wall_proc

puts @maze.within_n_steps_of [1, 1], n
