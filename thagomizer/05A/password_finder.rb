require 'digest'

data = File.read(ARGV[0]).strip

i = 0
password = ""

while password.length < 8
  digest = Digest::MD5.hexdigest "#{data}#{i}"

  if digest.start_with?("00000") then
    password << digest[5]
  end

  i += 1
end

puts password
