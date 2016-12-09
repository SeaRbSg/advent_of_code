class IPV7Address
  GOOD = /(\w*)/
  BAD  = /\[(\w*)\]/

  attr_accessor :address

  def self.contains_abba? str
    str.split('').each_cons(4) do |str|
      if str[0] == str[3] and
        str[1] == str[2] and
        str[0] != str[1]
        return true
      end
    end
  end

  def initialize address
    @address = address
  end

  def supports_tls?
    f_sections = @address.scan(/\[(\w*)\]/).select { |s| !s[0].empty? }.flatten
    t_sections = @address.scan(/(\w*)/).select { |s| !s[0].empty? }.flatten
    t_sections -= f_sections

    return false if f_sections.any? { |s| IPV7Address.contains_abba?(s) }

    return true  if t_sections.any? { |s| IPV7Address.contains_abba?(s) }

    false
  end
end

data = File.readlines(ARGV[0]).map { |d| IPV7Address.new d }

puts data.select { |d| d.supports_tls? }.length
