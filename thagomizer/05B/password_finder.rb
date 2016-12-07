require 'digest'

data = File.read(ARGV[0])

i           = 0
password    = "--------"

while password =~ /-/
  digest = Digest::MD5.hexdigest(data + i.to_s)

  if digest =~ /^00000/ and digest[5] =~ /[01234567]/ then
    pos = digest[5].to_i
    if password[pos] == "-" then
      password[pos] = digest[6]
    end
  end

  i += 1
end

puts password
