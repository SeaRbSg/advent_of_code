Node = Struct.new(*%i[x y size used avail use])

nodes = ARGF
  .read
  .scan(%r|/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s*(\d+)%|)
  .map {|match| Node.new(*match.map(&:to_i)) }

hash = {}
nodes.each do |node|
  hash[[node.x, node.y]] = node
end

max_x = nodes.map(&:x).max
# puts "max x: #{max_x}" # 36

puts nodes.permutation(2).select {|a,b|
  a.used != 0 && a.used <= b.avail
}.select {|a,b|
  [(a.x - b.x).abs <= 1& (a.y - b.y).abs <= 1
}.map {|a,b| "#{a.x},#{a.y} => #{b.x},#{b.y}" }.join("\n")

# Find a path from 0,36 to 0,0
