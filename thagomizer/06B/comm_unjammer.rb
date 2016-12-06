answers = File.readlines(ARGV[0]).map(&:chomp)

len     = answers.first.length
results = Hash.new { |h, k| h[k] = Array.new(len) { 0 } }

answers.each do |answer|
  answer.each_char.with_index do |v, i|
    results[v][i] += 1
  end
end

len.times do |i|
  print results.min_by { |bp, ary| ary[i] }.first
end

puts
