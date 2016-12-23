class Scrambler
  SWAP_POS = /swap position (\d+) with position (\d+)/
  SWAP_LETTERS = /swap letter (\w) with letter (\w)/
  REVERSE_POS = /reverse positions (\d+) through (\d+)/
  ROTATE_LR = /rotate (left|right) (\d+) steps?/
  ROTATE_LETTER = /rotate based on position of letter (\w)/
  MOVE_POS = /move position (\d+) to position (\d+)/

  ROTATE_LETTER_INDICES = { 1 => 0,
                            3 => 1,
                            5 => 2,
                            7 => 3,
                            2 => 4,
                            4 => 5,
                            6 => 6,
                            0 => 7 }

  attr_accessor :str, :instructions

  def initialize str, instructions
    @str = str
    @instructions = instructions
  end

  def rotate direction, steps
    steps.times do
      if direction == "right"
        @str = @str[-1] + @str[0...@str.length - 1]
      else
        @str = @str[1..@str.length] + @str[0]
      end
    end
  end

  def swap_pos x, y
    @str[x], @str[y] = @str[y], @str[x]
  end

  def scramble
    @instructions.each do |instruction|
puts @str
puts instruction
      case instruction
      when SWAP_POS
        swap_pos $1.to_i, $2.to_i
      when SWAP_LETTERS
        x = $1
        y = $2
        @str.gsub!(x, "@")
        @str.gsub!(y, x)
        @str.gsub!("@", y)
      when REVERSE_POS
        r = ($1.to_i..$2.to_i)

        @str[r] = @str[r].reverse
      when ROTATE_LR
        rotate $1, $2.to_i
      when ROTATE_LETTER
        i = @str.index($1)
        i += 1 if i >= 4
        i += 1
        rotate "right", i
      when MOVE_POS
        i = $1.to_i
        j = [$2.to_i, @str.length - 1].min
        c = @str[i]
        @str[i] = ""
        @str.insert j, c
      end
    end
puts @str
  end

  def unscramble
    @instructions.each do |instruction|
      case instruction
      when SWAP_POS
        swap_pos $1.to_i, $2.to_i
      when SWAP_LETTERS
        x = $1
        y = $2
        @str.gsub!(x, "@")
        @str.gsub!(y, x)
        @str.gsub!("@", y)
      when REVERSE_POS
        r = ($1.to_i..$2.to_i)

        @str[r] = @str[r].reverse
      when ROTATE_LR
        direction = if $1 == "left" then "right" else "left" end

        rotate direction, $2.to_i
      when ROTATE_LETTER
        l = $1
        i = @str.index(l)
        target_position = ROTATE_LETTER_INDICES[i]

        until @str[target_position] == l
          rotate "left", 1
        end
      when MOVE_POS
        i = $2.to_i
        j = [$1.to_i, @str.length - 1].min
        c = @str[i]
        @str[i] = ""
        @str.insert j, c
      end
    end
  end
end
