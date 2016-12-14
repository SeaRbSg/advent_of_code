require 'strscan'

class CompressedString
  COMPRESSION_MARKER = /\((\d+)x(\d+)\)/

  def initialize str
    @str = str
  end

  def decompress
    decompressed = ""

    scanner = StringScanner.new(@str)

    until scanner.eos?
      compression = scanner.scan(COMPRESSION_MARKER)
      if compression
        COMPRESSION_MARKER.match(compression).captures
        num_chars = $1.to_i
        repeats   = $2.to_i

        repeated_section = ""

        num_chars.times do
          repeated_section << scanner.getch
        end

        decompressed << repeated_section * repeats
      else
        decompressed << scanner.getch
      end
    end

    decompressed
  end

  def length
    decompress.length
  end
end
