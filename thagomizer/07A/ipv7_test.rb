require 'minitest/autorun'
require './ipv7.rb'

class IPV7Test < Minitest::Test
  def setup
    @address = IPV7Address.new("abba")
  end

  def test_contains_abba
    assert IPV7Address.contains_abba?("abba")
    refute IPV7Address.contains_abba?("aaaa")
    refute IPV7Address.contains_abba?("rhamaeovmbheijj")
  end
end
