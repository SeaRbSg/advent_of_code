def josephus n
  return 1 if n == 2
  return 3 if n == 3

  if n.even?
    2 * josephus(n / 2) - 1
  else
    2 * josephus(n / 2) + 1
  end
end

n = ARGV[0].to_i

puts josephus(n)
