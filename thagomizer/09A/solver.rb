require "./decompress.rb"

data = File.read(ARGV[0]).strip

puts CompressedString.new(data).length
