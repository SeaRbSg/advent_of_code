SAFE = '.'
TRAP = '^'

input     = File.read(ARGV[0]).strip
row_count = ARGV[1].to_i - 1
row_length = input.length

safe_tile_count = input.split('').select { |i| i == SAFE }.count
previous_row    = input

row_count.times do
  row = ""

  row_length.times do |i|

    left   = if i == 0 then SAFE else previous_row[i - 1] end
    center = previous_row[i]
    right  = previous_row[i + 1] || SAFE

    case [left, center, right]
    when [TRAP, TRAP, SAFE]
      row << TRAP
    when [SAFE, TRAP, TRAP]
      row << TRAP
    when [TRAP, SAFE, SAFE]
      row << TRAP
    when [SAFE, SAFE, TRAP]
      row << TRAP
    else
      safe_tile_count += 1
      row << SAFE
    end
  end

  previous_row = row
end

puts safe_tile_count
