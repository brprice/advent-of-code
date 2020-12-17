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

# part b is "do the same, but in 4 dimensions"
# it would be nice to write something generic in the number of dimensions,
# but we instead go for the quick solution of mostly-copy-and-paste...
function day17b(initialState)
  gens = 6
  # pad the input enough so to avoid boundary effects
  X, Y = size(initialState)
  current = Array{Bool,4}(undef,X+2*gens,Y+2*gens,1+2*gens,1+2*gens)
  for x in 1:X+2*gens, y in 1:Y+2*gens, z in 1:1+2*gens, w in 1:1+2*gens
    if z == 1+gens && w == 1+gens
      current[x,y,z,w] = get(initialState,(x-gens,y-gens),false)
    else
      current[x,y,z,w] = false
    end
  end
  for _ in 1:gens
    current = stepB(current)
  end
  count(current)
end

function getNbdB(A,x,y,z,w)
  nbd = []
  for dx in -1:1, dy in -1:1, dz in -1:1, dw in -1:1
    if !(dx==dy==dz==dw==0)
      push!(nbd,get(A,(x+dx,y+dy,z+dz,w+dw),false))
    end
  end
  return(nbd)
end

# This works on a finite grid, everything outside is considered inactive
function stepB(state)
  next = similar(state)
  X, Y, Z, W = size(next)
  for x in 1:X, y in 1:Y, z in 1:Z, w in 1:W
    c = state[x,y,z,w]
    n = count(getNbdB(state,x,y,z,w))
    if  (c && (n==2 || n==3)) || (!c && n==3)
      next[x,y,z,w] = true
    else
      next[x,y,z,w] = false
    end
  end
  return(next)
end

data = map(readlines("../../data/day17")) do l
  map(c->c=='#',collect(l))
end
data = hcat(data...)

println(day17a(data))
println(day17b(data))
