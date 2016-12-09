module Adventus
  REGX_SQ = /\[\w+\]/
  REGX_IP = /(\w+)?(#{REGX_SQ})+(\w+)?/

  def n_TLS_valid input
    input.split.count { |s| supports_TLS? s }
  end

  def n_SSL_valid input
    input.split.count { |s| supports_SSL? s }
  end

  def supports_TLS? ip
    bracketed, standard = filter ip
    return false if bracketed.any? { |w| is_ABBA? w }
    return true  if standard.any?  { |w| is_ABBA? w }
  end

  def supports_SSL? ip
    bracketed, standard = filter ip
    bracketed.map  { |w| get_ABA w }.flatten
             .any? { |s| standard.any? { |w| w.include? s[1]+s[0]+s[1] } }
  end

  def filter ip
    a, b = ip.scan(REGX_IP).flatten.compact.partition { |w| w.match(REGX_SQ) }
    a.map! { |s| s.delete!("[]") }
    [a, b]
  end

  def is_ABBA? str
    str.chars.each_cons(4) { |s| return true if s == s.reverse && s[0] != s[1] }
  end

  def get_ABA str
    str.chars.each_cons(3).select { |s| s == s.reverse && s[0] != s[1] }.map(&:join)
  end

end
