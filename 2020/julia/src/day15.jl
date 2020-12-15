# We speak numbers in a sequence, starting with a given prelude
# after that, a_{n+1} = 0 if a_n != a_i for all i<n
# and a_{n+1} = n-i if a_i == a_n and a_j != a_n for each i<j<n
# What is a_2020?
function day15a(prelude)
# We maintain a dictionary of the last time we spoke each number
  last = Dict(map(x->(x[2],x[1]),enumerate(prelude[1:end-1])))
  a = prelude[end]
  n = length(prelude)
  while n < 2020
    b = n - get(last,a,n)
    last[a] = n
    a = b
    n += 1
  end
  return a
end

data = map(n->parse(Int,n),split(read("../../data/day15",String),","))

println(day15a(data))
