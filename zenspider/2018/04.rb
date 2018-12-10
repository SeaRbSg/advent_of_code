#!/usr/bin/env ruby -w

module Enumerable
  def count_by &blk
    group_by(&blk).transform_values(&:count)
  end
end

data    = Hash.new { |h,k| h[k] = [] }
date_re = '\d+-\d+-\d+ \d+:(\d+)'
current = nil

ARGF.read.lines.sort.each do |line|
  case line
  when /\[#{date_re}\] Guard #(\d+) begins shift/ then
    current = $2.to_i
  when /\[#{date_re}\] falls asleep/ then
    data[current] << $1.to_i
  when /\[#{date_re}\] wakes up/ then
    minutes = data[current].pop...$1.to_i
    data[current].concat minutes.to_a
    data[current].sort!                     # just for me
  end
end

# 04a:
guard, _count = data.transform_values { |v| v.count }.max_by(&:last)
data.transform_values! { |v| v.count_by(&:itself) } # { guard => { minute => count } }
minute, _count =  data[guard].max_by(&:last)

p guard * minute

# 04b:
# relying on previous transform_values!

# { guard => [max_minute, max_count] }
id, (min, _) = data.transform_values { |v| v.max_by(&:last) }.max_by { |_,(_,v)| v }
p id * min

__END__
[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up
