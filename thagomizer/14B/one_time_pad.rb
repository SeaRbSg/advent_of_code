require 'digest'

class PasswordGenerator
  HASHINGS = 2016

  attr_accessor :password_indices

  def initialize salt
    @salt             =    salt
    @hashes           = {}
    @password_indices = []
  end

  def calculate_stretched_hash i
    str = Digest::MD5.hexdigest "#{@salt}#{i}"

    HASHINGS.times do
      str = Digest::MD5.hexdigest(str)
    end

    str
  end

  def generate_n_passwords n
    n -= 1   # Fencepost
    return if password_indices[n]

    i = password_indices.last || 0

    until password_indices[n] do
      @hashes[i] ||= calculate_stretched_hash i

      if @hashes[i] =~ /(\w)\1\1/ then
        to_find = $1 * 5

        (1..1_000).each do |j|
          @hashes[i + j] ||= calculate_stretched_hash i + j

          if @hashes[i + j].include? to_find then
            @password_indices << i
            break
          end
        end
      end

      i += 1   # IMPORTANT!
    end
  end
end

salt = File.read(ARGV[0]).strip

pg = PasswordGenerator.new salt

pg.generate_n_passwords 64

puts pg.password_indices.last
