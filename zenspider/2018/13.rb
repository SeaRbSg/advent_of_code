#!/usr/bin/env ruby -w

input = if ARGV.empty? then
          DATA
        else
          ARGF
        end

class Runner
  attr_accessor :state

  def initialize track
    self.state = track.split(/\n/).map { |s|
      s.chars.to_a.map { |c|
        case c
        when "<", ">" then
          ["-", c]
        when "^", "v" then
          ["|", c]
        else
          [c]
        end
      }
    }
  end

  def tick
    ns = Marshal.load Marshal.dump state # deep clone? dunno if needed yet

    state.each_with_index do |line, row|
      line.each_with_index do |c, col|
        if c.size == 1 then
          # do nothing
        else
          nsc = ns[row][col]

          p [row, col, nsc]

          actor = nsc.pop

          case actor
          when "v" then
            abort "moving to #{row+1}, #{col}"
            abort "collision #{row+1}, #{col}" if ns[row+1][col].size > 1
            ns[row+1][col].push actor # TODO: change actor direction
          when "^" then
            abort "moving to #{row+1}, #{col}"
            abort "collision #{row+1}, #{col}" if ns[row-1][col].size > 1
            ns[row-1][col].push actor
          when "<" then
            abort "moving to #{row}, #{col-1}"
            abort "collision #{row}, #{col-1}" if ns[row][col-1].size > 1
            ns[row][col-1].push actor
          when ">" then
            abort "moving to #{row}, #{col+1}"
            abort "collision #{row}, #{col+1}" if ns[row][col+1].size > 1
            ns[row][col+1].push actor
          else
            warn "unparsed: #{c.inspect}"
          end
        end
      end
    end
  end

  def run
    tick
  end
end

Runner.new(input.read).run

__END__
/->-\        
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/   
