# 3d game of life
# part a: only 6 generations, on a small grid
# initial state is contained within one plane
function day17a(initialState)
  gens = 6
  # pad the input enough so to avoid boundary effects
  w, h = size(initialState)
  current = Array{Bool,3}(undef,w+2*gens,h+2*gens,1+2*gens)
  for x in 1:w+2*gens, y in 1:h+2*gens, z in 1:1+2*gens
    if z == 1+gens
      current[x,y,z] = get(initialState,(x-gens,y-gens),false)
    else
      current[x,y,z] = false
    end
  end
  for _ in 1:gens
    current = step(current)
  end
  count(current)
end

function getNbd(A,x,y,z)
  nbd = []
  for dx in -1:1, dy in -1:1, dz in -1:1
    if !(dx==dy==dz==0)
      push!(nbd,get(A,(x+dx,y+dy,z+dz),false))
    end
  end
  return(nbd)
end

# This works on a finite grid, everything outside is considered inactive
function step(state)
  next = similar(state)
  w, h, d = size(next)
  for x in 1:w, y in 1:h, z in 1:d
    c = state[x,y,z]
    n = count(getNbd(state,x,y,z))
    if  (c && (n==2 || n==3)) || (!c && n==3)
      next[x,y,z] = true
    else
      next[x,y,z] = false
    end
  end
  return(next)
end

data = map(readlines("../../data/day17")) do l
  map(c->c=='#',collect(l))
end
data = hcat(data...)

println(day17a(data))
