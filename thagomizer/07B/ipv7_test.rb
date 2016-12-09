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

  def test_find_all_aba
    assert_equal [["a", "b"]], IPV7Address.find_all_aba("aba")
    assert_equal [["z", "a"], ["z", "b"]], IPV7Address.find_all_aba("zazbz")

    assert_equal [], IPV7Address.find_all_aba("aaaa")
    assert_equal [], IPV7Address.find_all_aba("rhameovmbheijj")
  end
end
