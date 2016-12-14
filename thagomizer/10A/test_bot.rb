require 'minitest/autorun'
require './bot.rb'

class BotTest < Minitest::Test
  def setup
    @bot = Bot.new
  end

  def test_receive_chip
    @bot.receive_chip 3

    assert_equal [3], @bot.chips

    @bot.receive_chip 5

    assert_equal [3, 5], @bot.chips

    assert_raises(RuntimeError) { @bot.receive_chip(10) }
  end
end
