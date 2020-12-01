function getPairSum(target,nums)
  entries = Set(nums)
  for i in nums
    if in(target-i,entries)
      return (i,target-i)
    end
  end
  return nothing
end

function day1a(expenses)
  p = getPairSum(2020,expenses)
  if p === nothing
    error("day1a: did not find a pair summing to 2020")
  end
  (x,y) = p
  x*y
end

function getTripleSum(target,nums)
  for i in nums
    p = getPairSum(target-i,nums)
    if p !== nothing
      (x,y)=p
      return (i,x,y)
    end
  end
  return nothing
end

function day1b(expenses)
  t = getTripleSum(2020,expenses)
  if t === nothing
    error("day1b: did not find a triple summing to 2020")
  end
  (x,y,z) = t
  x * y * z
end


expenses=parse.(Int,readlines("../../data/day1"))

print("Day 1a: ")
println(day1a(expenses))

print("Day 1b: ")
println(day1b(expenses))
