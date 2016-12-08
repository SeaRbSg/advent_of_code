module Adventus
  ALPHABT = "abcdefghijklmnopqrstuvwxyz"
  ALPHA_N = ALPHABT.size
  KEYWORD = "northpole"

  def sum_of_all_valid_room_id bunch
    bunch.split.map { |e| decrypt(e).last }.reduce(:+)
  end

  def find_keyword_room bunch
    bunch.split("\n").each do |e|
      msg, n = decrypt(e)
      return n if n && msg.include?(KEYWORD)
    end
  end

  def decrypt stringenous
    msg, chksum, sectid = parse_d4 stringenous

    h = most_frequent_chars_ordered msg.join.chars

    return [decypher_room(msg, sectid), sectid] if authentic? chksum, h
    ["", 0]
  end

  def parse_d4 stringenous
    wip = stringenous.scan(/(\w+)+/)
    chksum = wip.pop.first.chars.take(5)
    sectid = wip.pop.first.to_i
    [wip, chksum, sectid]
  end

  def most_frequent_chars_ordered arr
    h = {}
    arr.uniq.map do |c|
      n = arr.count(c)
      h[n] ? (h[n] << c) : (h[n] = [c])
    end
    Hash[h.sort.reverse]
  end

  def authentic? chksum, freq_chars
    chksum.each do |c|
      letter_set = freq_chars.first[1]
      return false unless letter_set.include? c
      letter_set.delete c
      freq_chars.shift if letter_set.empty?
    end
    true
  end

  def decypher_room words, n
    words.flatten(1).map do |w|
      w.chars.map { |c| decypher_char(c, n) }.join
    end.join(" ")
  end

  def decypher_char letter, n
    letter == " " ? letter : ALPHABT[(ALPHABT.index(letter) + n) % ALPHA_N]
  end

end
