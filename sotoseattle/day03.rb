module Adventus

  def count_triangles parsed_input
    parsed_input.count do |row|
      arr = row.sort
      arr.pop < arr.reduce(:+)
    end
  end

  def parse_by_rows input
    input.split("\n").map { |row| row.strip.split.map(&:to_i) }
  end

  def parse_by_cols input
    parse_by_rows(input).each_slice(3).map(&:transpose).flatten(1)
  end

  def triangles_in_rows input
    count_triangles parse_by_rows input
  end

  def triangles_in_cols input
    count_triangles parse_by_cols input
  end

end
