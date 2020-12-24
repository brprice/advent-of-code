# Coordinate convention: working on a hex grid,
#      (0,1)   (1,1)
# (-1,0)   (0,0)   (1,0)
#     (-1,-1)  (0,-1)

# Look at the end point of each path, how many
# of those points appear an odd number of times
# as an endpoint?
function day24a(tiles)
  return count(values(tiles))
end

# "true" means "is black"
function tiles(paths)
  ends = Dict()
  for e in endpoint.(paths)
    if in(e,keys(ends))
      ends[e] = !ends[e]
    else
      ends[e] = true
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
# point it seems rather pointless!
function day24b(tiles)
# I didn't know whether off-the-map tiles should be flipped,
# but it turns out they do need to be (i.e. we are working on
# an infinite grid)
# I can get away with only keeping track of the black tiles if so,
# and thus not rewriting as much from dayA.
# This should also be quicker, as the black tiles are sparser
# (alternately, I could just drop the white tiles before looping,
# changing our runtime from ~7s to ~1s)
# TODO: the above!
  for _ in 1:100
    adjs = Dict()
    # We are careful to record adjs[x] = 0 where appropriate
    # (rather than leaving that key out)
    for kv in tiles
      k = kv.first
      v = kv.second
      for d in values(dir)
        kd = k.+d
        b = v ? 1 : 0
        adjs[kd] = b + get(adjs,kd,0)
      end
    end
    next = Dict()
    for kv in adjs
      k = kv.first
      bns = kv.second
      v = get(tiles,k,false)
      if v && (bns == 0 || bns > 2)
        next[k] = false
      elseif !v && bns == 2
        next[k] = true
      else
        next[k] = v
      end
    end
    # don't keep track of all the white tiles
    tiles = filter(x->x.second,next)
  end
  day24a(tiles)
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

ts = tiles(data)
println(day24a(ts))
println(day24b(ts))
