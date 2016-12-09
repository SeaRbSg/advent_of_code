module Adventus
  REGX_SQ = /\[(\w+)\]/
  REGX_IP = /(\w+)?(\[\w+\])+(\w+)?/

  def n_TLS_valid input
    input.split.count { |s| supports_TLS?(s) }
  end

  def supports_TLS? ip
    squared , naked = ip.scan(REGX_IP).flatten.compact
                        .partition { |w| w.match(REGX_SQ) }

    return false if squared.any? { |w| is_ABBA? w.delete("[]") }
    return true  if naked.any? { |w| is_ABBA? w }
  end

  def is_ABBA? str
    str.chars.each_cons(4) do |s|
      return true if s == s.reverse and s[0] != s[1]
    end
  end

end
