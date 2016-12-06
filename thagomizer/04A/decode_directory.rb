class BuildingDirectory
  DIRECTORY_REGEX = /([\w-]+)(\w{3})\[(\w{5})\]/

  def initialize path
    @directory = File.readlines(path)
    @real_room_sectors = []
  end

  def sort_rooms a, b
    if a[1] == b[1]
      a[0] <=> b[0]
    else
      b[1] <=> a[1]   # Invert for decreasing sort
    end
  end

  def decode
    @directory.each do |entry|
      room_name, sector, check_sum = DIRECTORY_REGEX.match(entry).captures

      room_name = room_name.gsub("-", "").split('')

      freqs = Hash[room_name.group_by { |c| c }.map { |k, v| [k, v.size] }]
      check = freqs.sort { |a, b| sort_rooms(a, b) }[0...5].map(&:first).join

      if check == check_sum then
        @real_room_sectors << sector
      end
    end
  end

  def sector_sum
    @real_room_sectors.map(&:to_i).inject(:+)
  end
end


bd = BuildingDirectory.new("data.txt")

bd.decode
puts bd.sector_sum
