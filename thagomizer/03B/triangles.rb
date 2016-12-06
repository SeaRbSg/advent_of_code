require "pp"

def possible? a, b, c
  a + b > c and b + c > a and a + c > b
end

data = File.readlines(ARGV[0]).map { |l| l.strip.split(/\s+/).map(&:to_i) }
triangles = []

data.each_slice(3) do |x, y, z|
  (0..2).each do |i|
    triangles << [x[i], y[i], z[i]]
  end
end

puts triangles.select { |tri| possible?(*tri) }.length
