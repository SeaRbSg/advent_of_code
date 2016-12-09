require 'minitest/autorun'
require 'minitest/pride'

require_relative './day01.rb'
require_relative './day02.rb'
require_relative './day03.rb'
require_relative './day04.rb'
require_relative './day05.rb'
require_relative './day06.rb'
require_relative './day07.rb'

class TestAdventus < Minitest::Test
  include Adventus

  def test_day01
    long_input = File.read("./test_files/input_day1.txt")

    assert_equal  2,  distance_in_blocks("R2, R2, R2")
    assert_equal  5,  distance_in_blocks("R2, L3")
    assert_equal 12,  distance_in_blocks("R5, L5, R5, R3")
    assert_equal 146, distance_in_blocks(long_input)

    assert_equal  4,  distance_to_twice_visited_spot("R8, R4, R4, R8")
    assert_equal 131, distance_to_twice_visited_spot(long_input)
   end

  def test_day02
    long_input = File.read("./test_files/input_day2.txt")

    assert_equal "1985",  Square_Keypad.new.decypher("ULL\nRRDDD\nLURDL\nUUUUD")
    assert_equal "78985", Square_Keypad.new.decypher(long_input)

    assert_equal "5DB3",  Diamond_Keypad.new.decypher("ULL\nRRDDD\nLURDL\nUUUUD")
    assert_equal "57DD8", Diamond_Keypad.new.decypher(long_input)
  end

  def test_day03
    long_input = File.read("./test_files/input_day3.txt")

    assert_equal 0, triangles_in_rows("  25  5  10 \n  5  25  10 \n  10  25  5 ")

    assert_equal 993,  triangles_in_rows(long_input)
    assert_equal 1849, triangles_in_cols(long_input)
  end

  def test_day04
    long_input = File.read("./test_files/input_day4.txt")

    assert_equal ["ttttt uuu s r q", 123], decrypt("aaaaa-bbb-z-y-x-123[abxyz]")
    assert_equal ["z a b c d e f g", 987], decrypt("a-b-c-d-e-f-g-h-987[abcde]")
    assert_equal ["bch o fsoz fcca", 404], decrypt("not-a-real-room-404[oarel]")
    assert_equal ["", 0], decrypt("totally-real-room-200[decoy]")
    assert_equal 409147, sum_of_all_valid_room_id(long_input)

    assert_equal "v", decypher_char("q", 343)
    assert_equal 991, find_keyword_room(long_input)
  end

  def test_day05
    skip # Too slow
    assert_equal "18f47a30",  hashit_out("abc")
    assert_equal "c6697b55",  hashit_out("ffykfhsq")

    assert_equal "05ace8e3",  hashit_better("abc")
    assert_equal "8c35d1ab",  hashit_better("ffykfhsq") #6c35d1ab
  end

  def test_day06
    short_input = "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\n
                   rasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\n
                   dvrsen\nenarar"
    long_input = File.read("./test_files/input_day6.txt")

    assert_equal "easter", decode_by_repetition(short_input)
    assert_equal "advent", decode_by_least_common(short_input)

    assert_equal "qtbjqiuq", decode_by_repetition(long_input)
    assert_equal "akothqli", decode_by_least_common(long_input)

  end

  def test_day07
    long_input = File.read("./test_files/input_day7.txt")

    assert supports_TLS?("abba[mnop]qrst[ururur]ksdhksd")
    assert supports_TLS?("abba[mnop]qrst")
    refute supports_TLS?("abcd[bddb]xyyx")
    refute supports_TLS?("aaaa[qwer]tyui")
    assert supports_TLS?("ioxxoj[asdfgh]zxcvbn")

    assert_equal 118, n_TLS_valid(long_input)
  end
end
