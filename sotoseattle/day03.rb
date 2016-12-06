module Adventus

  def valid_triangles_by_row input
    count_triangles parse_d3(input)
  end

  def valid_triangles_by_col input
    count_triangles transpose_cols_each_3_rows(input)
  end

  def parse_d3 input
    input.split("\n").map { |row| row.strip.split.map(&:to_i) }
  end

  def transpose_cols_each_3_rows input
    parse_d3(input).each_slice(3).map(&:transpose).flatten(1)
  end

  def count_triangles parsed_input
    parsed_input.count do |row|
      arr = row.sort
      arr.pop < arr.reduce(:+)
    end
  end

end
