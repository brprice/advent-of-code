function day10a(data)
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

function day10b(data)
  numPathsTo = Dict(0=>1)
  target = data[end]+3
  push!(data,target)
  for i in data
    numPathsTo[i] = get(numPathsTo, i-3, 0) + get(numPathsTo, i-2, 0) + get(numPathsTo, i-1, 0)
  end
  return numPathsTo[target]
end

data = map(x->parse(Int,x),readlines("../../data/day10"))
sort!(data)

println("Day 10a: ",day10a(data))
println("Day 10b: ",day10b(data))
