class Bot
  attr_accessor :chips, :low, :high

  def initialize
    @chips = []
    @low   = nil
    @high  = nil
  end

  def receive_chip value
    raise "Already have two chips" if @chips.length == 2
    @chips << value
    @chips.sort!
  end

  def tick
    return unless @chips.length == 2
    raise "WIRING ERROR" unless @low and @high

    min, max = @chips.minmax
    @low.receive_chip min
    @high.receive_chip max
    @chips = []
  end

  def to_s
    chips.join(", ")
  end
end
