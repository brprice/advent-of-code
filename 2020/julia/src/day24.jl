# Coordinate convention: working on a hex grid,
#      (0,1)   (1,1)
# (-1,0)   (0,0)   (1,0)
#     (-1,-1)  (0,-1)

# Look at the end point of each path, how many
# of those points appear an odd number of times
# as an endpoint?
function day24a(black)
  return length(black)
end

function blackTiles(paths) :: Set{Tuple{Int,Int}}
  ends = Set()
  for e in endpoint.(paths)
    if in(e,ends)
      delete!(ends,e)
    else
      push!(ends,e)
    end
  end
  return ends
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

# Another cellular automaton
# I should really generalise this code and reuse it in
# the three(?) days that have needed it, but by this
# point it seems rather pointless
function day24b(black)
# Contrary to julia's claim that it usually inferrs all
# types, so type annotations don't improve speed,
# the two in this function (adjs and next) do make a
# large difference (from ~.8s to ~.3s, in julia 1.0.4)
  for _ in 1:100
    adjs = Dict{Tuple{Int,Int},Int}()
    for l in black
      for d in values(dir)
        ld = l .+ d
        adjs[ld] = 1 + get(adjs,ld,0)
      end
    end
    # NB: black tiles with 0 black neighbours turn white
    # but they do not appear in adjs, so we obey this rule
    # by just not doing anything special here!
    next = Set{Tuple{Int,Int}}()
    for kv in adjs
      k = kv.first
      bns = kv.second
      isblack = in(k,black)
      if (isblack && (bns == 1 || bns == 2)) || (!isblack && bns == 2)
        push!(next,k)
      end
    end
    black = next
  end
  length(black)
end

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

black = blackTiles(data)
println(day24a(black))
println(day24b(black))
