class Screen
  def initialize width = 50, height = 6
    @height = height
    @width  = width
    @screen = []

    height.times do
      @screen << Array.new(width, ".")
    end
  end

  def to_s
    @screen.map { |row| row.join }.join("\n")
  end

  def rect columns, rows
    rows.times do |i|
      columns.times do |j|
        @screen[i][j] = "#"
      end
    end
  end

  def rotate_row row, offset
    offset.times do
      @screen[row].unshift(@screen[row].pop)
    end
  end

  def rotate_column col, offset
    current_col = @screen.map { |row| row[col] }

    offset.times do
      current_col.unshift(current_col.pop)
    end

    @height.times do |r|
      @screen[r][col] = current_col[r]
    end
  end

  def count_pixels
    @screen.map { |r| r.select { |p| p == "#" }.count }.inject(:+)
  end
end
