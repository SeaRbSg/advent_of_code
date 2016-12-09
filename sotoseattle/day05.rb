module Adventus
  require 'digest/md5'

  def hashit_out something
    n = 0
    sol = ""
    while sol.size < 8
      chars, n  = next_00000_MD5 something, n
      sol += chars[0]
    end
    sol
  end

  def hashit_better something
    n = 0
    sol = Array.new(8, "_")
    while sol.include? "_"
      chars, n  = next_00000_MD5 something, n
      m = chars[0].to_i
      if chars[0] =~ /[0-7]/ and sol[m] == "_"
        sol[m] = chars[1]
      end
    end
    sol.join
  end

  def next_00000_MD5 str, n
    while true do
      n += 1
      hash = Digest::MD5.hexdigest("#{str}#{n}")
      return [hash[5,2], n] if hash[0, 5] == "00000"
    end
  end

end
