require 'digest'

data = File.read(ARGV[0]).strip

passwords = []
$hashes = {}

i = 0

while passwords.length < 64 do
  $hashes[i] ||= Digest::MD5.hexdigest "#{data}#{i}"

  if $hashes[i] =~ /(\w)\1\1/ then
    to_find = $1 * 5

    (1..1_000).each do |j|
      $hashes[i + j] ||= Digest::MD5.hexdigest "#{data}#{i + j}"

      if $hashes[i + j].include? to_find then
        passwords << i
        break
      end
    end
  end

  i += 1   # IMPORTANT!
end

puts passwords.last
