file = ARGV[0]
min  = ARGV[1].to_i
max  = ARGV[2].to_i

data = File.readlines(file)

data.map! do |line|
  line =~ /(\d+)-(\d+)/
  low  = $1.to_i
  high = $2.to_i

  [low, high]
end

data.sort!

allowed_count = 0
current_low  = 0
current_high = 0

data.each do |low, high|
  if low > current_high + 1
    current_low = low
    allowed_count += (current_low - current_high - 1)
  end

  current_high = high if high > current_high
end

allowed_count += max - current_high

puts allowed_count
