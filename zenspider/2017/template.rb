#!/usr/bin/env ruby -w

require "../2017/utils.rb"

# DESCRIPTION

class ProblemNNa
  def run input
  end
end

class ProblemNNb < ProblemNNa
end

if ARGV.empty? then
  require "minitest/autorun"

  class TestNN < Minitest::Test
    def test_a
      flunk
    end

    def test_b
      skip
    end
  end
else
  input = ARGF.read.chomp
  p ProblemNNa.new.run input
  p ProblemNNb.new.run input
end
