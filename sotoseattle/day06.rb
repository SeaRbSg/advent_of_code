module Adventus

  def decode_by_repetition msg
    decode msg, :max_by
  end

  def decode_by_least_common msg
    decode msg, :min_by
  end

  def decode msg, cypher
    msg.split.map(&:chars).transpose.map do |v|
      v.group_by(&:itself).values.send(cypher, &:size).first
    end.join
  end

end
