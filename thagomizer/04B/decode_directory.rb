require "pp"

class BuildingDirectory
  DIRECTORY_REGEX = /([\w-]+)(\w{3})\[(\w{5})\]/

  def initialize path
    @directory = File.readlines(path)
    @room_names = []
  end

  def rotate char, rotations
    rotations.times do
      case char
      when "-"
        return " "
      when "z"
        char = "a"
      else
        char = char.next
      end
    end
    char
  end

  def sort_rooms a, b
    if a[1] == b[1]
      a[0] <=> b[0]
    else
      b[1] <=> a[1]   # Invert for decreasing sort
    end
  end

  def decode
    @room_names = @directory.map { |entry|
      room_name, sector, _ = DIRECTORY_REGEX.match(entry).captures

      [room_name.each_char.map { |c| rotate(c, sector.to_i)}.join, sector]
    }
    Hash[@room_names]
  end

  def find_by_name to_find
    @room_names.each do |name, sector|
      return sector if name.include?(to_find)
    end
    "not found"
  end

end


bd = BuildingDirectory.new("data.txt")

bd.decode
puts bd.find_by_name("northpole")
