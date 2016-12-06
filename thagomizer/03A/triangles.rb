def possible? a, b, c
  a + b > c and b + c > a and a + c > b
end


data = File.readlines(ARGV[0]).map { |l| l.strip.split(/\s+/).map(&:to_i) }
puts data.select { |tri| possible?(*tri) }.length
