def lines_of_numbers input
  input.lines.map(&:integers)
end

class String
  def integers
    self.scan(/-?\d+/).map(&:to_i)
  end
end
