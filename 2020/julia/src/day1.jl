function getPairSum(target,nums)
  entries = Set(nums)
  for i in nums
    if in(target-i,entries)
      return (i,target-i)
    end
  end
  error("getPairSum: did not find a pair with the correct sum")
end

function day1a(expenses)
  (x,y) = getPairSum(2020,expenses)
  x*y
end


expenses=parse.(Int,readlines("../../data/day1"))

print("Day 1a: ")
day1a(expenses)
