# Coordinate convention: working on a hex grid,
#      (0,1)   (1,1)
# (-1,0)   (0,0)   (1,0)
#     (-1,-1)  (0,-1)

# Look at the end point of each path, how many
# of those points appear an odd number of times
# as an endpoint?
function day24a(paths)
  ends = Set()
  for e in endpoint.(paths)
    if in(e,ends)
      delete!(ends,e)
    else
      push!(ends,e)
    end
  end
  return length(ends)
end

function endpoint(path)
  c = (0,0)
  for p in path
    c = c .+ dir[p]
  end
  return c
end

# NB: "const" does not do what I would expect - see the julia manual.
# In particular, we are allowed to mutate the dictionary (but we will
# not do so!).
const dir = Dict(
  "nw" => (0,1),
  "ne" => (1,1),
  "w" => (-1,0),
  "e" => (1,0),
  "sw" => (-1,-1),
  "se" => (0,-1)
)

data = map(readlines("../../data/day24")) do line
  dirs = String[]
  i = 1
  l = length(line)
  while i <= l
    if in(line[i],"ns")
      push!(dirs,line[i:i+1])
      i += 2
    else
      push!(dirs,line[i:i])
      i += 1
    end
  end
  return dirs
end

println(day24a(data))
