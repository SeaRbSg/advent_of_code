module Adventus

  def sector_ID_sum bunch
    bunch.split.map {|e| sector_id e}.reduce(:+)
  end

  def sector_id stringenous
    wip = stringenous.scan(/(\w+)+/)
    chksum = wip.pop.first.chars.take(5)
    sectid = wip.pop.first.to_i
    wip = wip.join.chars

    h = {}
    wip.uniq.map do |c|
      n = wip.count(c)
      h[n] = [] unless h[n]
      h[n] << c
    end
    h = Hash[h.sort.reverse]

    chksum.each do |c|
      if h.first[1].include? c
        h.first[1].delete c
        h.shift if h.first[1].empty?
      else
        return 0
      end
    end

    sectid
  end

end
