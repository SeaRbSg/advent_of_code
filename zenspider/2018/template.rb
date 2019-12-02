#!/usr/bin/env ruby -w

DATA.each_line do |line|
  case line
  when /fuck/ then
    # do nothing
  else
    warn "unparsed: #{line.chomp}"
  end
end

__END__
... data ...
