data = File.read(ARGV[0]).strip
disk_length = ARGV[1].to_i

# Generate String
while data.length < disk_length do
  b = data.chars.map { |c| c == "1" ? "0" : "1" }.reverse.join

  data = data + "0" + b
end

data = data[0...disk_length]

# Checksum
checksum = ""

checksum = data

begin
  new_checksum = ""
  checksum.chars.each_slice(2) do |a, b|
    if a == b
      new_checksum << "1"
    else
      new_checksum << "0"
    end
  end
  checksum = new_checksum
end until checksum.length % 2 == 1

puts checksum
