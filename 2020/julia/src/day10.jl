function day10a(data)
  sort!(data)
  last = 0
  ones = 0
  threes = 0
  for i in data
    if i-last == 1
      ones += 1
    elseif i-last == 3
      threes += 1
    end
    last = i
  end
  return ones*(threes+1)
end

data = map(x->parse(Int,x),readlines("../../data/day10"))

println("Day 10a: ",day10a(data))
