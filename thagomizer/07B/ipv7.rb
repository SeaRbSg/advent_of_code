class IPV7Address
  SUPER = /(\w*)/
  HYPER  = /\[(\w*)\]/

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

  def self.find_all_aba str
    abas = []

    str.split('').each_cons(3) do |str|
      if str[0] == str[2] and
        str[0] != str[1]
        abas << [str[0], str[1]]
      end
    end

    abas
  end

  def initialize address
    @address = address
  end

  def supports_tls?
    h_sections = @address.scan(HYPER).select { |s| !s[0].empty? }.flatten
    s_sections = @address.scan(SUPER).select { |s| !s[0].empty? }.flatten
    s_sections -= h_sections

    return false if h_sections.any? { |s| IPV7Address.contains_abba?(s) }

    return true  if s_sections.any? { |s| IPV7Address.contains_abba?(s) }

    false
  end

  def supports_ssl?
    h_sections = @address.scan(HYPER).select { |s| !s[0].empty? }.flatten
    s_sections = @address.scan(SUPER).select { |s| !s[0].empty? }.flatten
    s_sections -= h_sections

    s_sections.each do |s|
      IPV7Address.find_all_aba(s).each do |a, b|
        return true if h_sections.any? { |h| h.include?("#{b}#{a}#{b}") }
      end
    end

    false
  end
end

if ARGV[0] then
  data = File.readlines(ARGV[0]).map { |d| IPV7Address.new d }

  puts data.select { |d| d.supports_ssl? }.length
end
