# Bus with id i leaves at times divisible by i
# Find the next bus to leave
# (if i=0, ignore it)
function day13a(time,ids0)
  ids = filter(x->x!=0,ids0)
  tis = map(i->(i-time%i,i),ids)
  t, i = minimum(tis)
  i*t
end

# For a list of ids i_n (zero-indexed), find the least t such that
# each bus departs at t+n.
# A bus with id i leaves at times divisible by i, if i=0, no restriction on
# busses departing at t+n
function day13b(ids)
# This is just chinese remainder theorem
# (We assume that the ids are pairwise coprime)
  constraints = []
  for (d,i) in enumerate(ids)
    if i != 0
      push!(constraints,(-(d-1),i))
    end
  end
  cr(constraints)
end

# If cs = [(a_1,n_1),...], solve the simultaneous equations
# x = a_i (mod n_i)
# We assume the n_i are pairwise coprime
function cr(cs)
# This is the simple, direct construction, rather than the more
# efficient inductive do-a-pair-of-equations-at-a-time solution.
# More than good enough for what we are doing.
  N = prod(map(x->x[2],cs))
  x = sum(map(an->an[1]*div(N,an[2])*invmod(div(N,an[2]),an[2]),cs))
  mod(x,N)
end


# we convert 'x' to 0
data = readlines("../../data/day13")
time = parse(Int,data[1])
ids = map(split(data[2],",")) do x
  if x == "x"
    return 0
  end
  parse(Int,x)
end

println("Day 13a: ",day13a(time,ids))
println("Day 13b: ",day13b(ids))
