require 'benchmark/ips'

ARY = (0...1_000).to_a

Benchmark.ips do |x|
  # Configure the number of seconds used during
  # the warmup phase (default 2) and calculation phase (default 5)
  x.config(:time => 5, :warmup => 2)

  # Typical mode, runs the block as many times as it can

  # x.report("null") do |n|
  #   n.times do
  #     ary = ARY.dup
  #   end
  # end

  x.report("subtraction") do |n|
    n.times do
      ary = ARY.dup
      ary = ary - [101]
    end
  end

  x.report("delete") do |n|
    n.times do
      ary = ARY.dup
      ary.delete(101)
    end
  end

  # Compare the iterations per second of the various reports!
  x.compare!
end
