module Adventus
  REGX_SQ = /\[(\w+)\]/
  REGX_IP = /(\w+)?(\[\w+\])+(\w+)?/

  def n_TLS_valid input
    input.split.count { |s| supports_TLS?(s) }
  end

  def supports_TLS? ip
    bracketed , standard = ip.scan(REGX_IP).flatten.compact.partition { |w| w.match(REGX_SQ) }

    return false if braketed.any? { |w| is_ABBA? w.delete("[]") }
    return true  if standard.any? { |w| is_ABBA? w }
  end

  def is_ABBA? str
    str.chars.each_cons(4) { |s| return true if s == s.reverse and s[0] != s[1] }
  end

  def supports_SSL? ip
    str = ip.delete("[]")
    str.chars.each_cons(3).select { |s| s.first == s.last and s[0] != s[1] }
                          .any?   { |s| str.include? s[1]+s[0]+s[1] }
  end

end
