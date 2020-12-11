function day11a(data)
  day11(data,getNbdA,4)
end

function day11b(data)
  day11(data,getNbdB,5)
end

# basically game of life
function day11(data,nbd,lim)
  (h,w)=size(data)
  while true
    next = step(data,nbd,lim)
    if next==data
      break
    end
    data=next
  end
  count(x->x==OCCUPIED,data)
end

function step(data,extractNbd,lim)
  (h,w)=size(data)
  next = similar(data)
  for i in 1:h, j in 1:w
    if data[i,j] == FLOOR
      next[i,j] = FLOOR
      continue
    end
    nbd=extractNbd(data,i,j)
    o=count(x->x==OCCUPIED,nbd)
    if data[i,j] == EMPTY && o == 0
      next[i,j] = OCCUPIED
    elseif data[i,j] == OCCUPIED && o >= lim
      next[i,j] = EMPTY
    else
      next[i,j] = data[i,j]
    end
  end
  next
end

# The 8 surrounding cells (fewer if at a boundary)
function getNbdA(data,i,j)
  nbd = []
  for di in -1:1, dj in -1:1
    if di==0==dj
      continue
    end
    d=get(data,(i+di,j+dj),missing)
    if d!==missing
      push!(nbd,d)
    end
  end
  nbd
end

# the first non-floor in the 8 directions (less if there is only floor)
function getNbdB(data,i,j)
  nbd = []
  for di in -1:1, dj in -1:1
    if di==0==dj
      continue
    end
    tgt=(i,j)
    d=FLOOR
    while d!==missing
      tgt = tgt.+(di,dj)
      d=get(data,tgt,missing)
      # NB: isequal(FLOOR,nothing) is false, but FLOOR==nothing is nothing
      if isequal(d,EMPTY) || isequal(d,OCCUPIED)
        push!(nbd,d)
        break
      end
    end
  end
  nbd
end

@enum Cell FLOOR EMPTY OCCUPIED

function Base.show(io::IO, c::Cell)
  if c==FLOOR
    print(io,'.')
  elseif c==EMPTY
    print(io,'L')
  elseif c==OCCUPIED
    print(io,'#')
  end
end

# Hmm, must be a better way to read in the data than this
# I can't use readdlm as the input data has no delimiters!
# (And I don't want to pre-process it)
datatmp = []
map(readlines("../../data/day11")) do l
  dl = []
  for c in l
    if c==='L'
      push!(dl,EMPTY)
    elseif c==='#'
      push!(dl,OCCUPIED)
    elseif c==='.'
      push!(dl,FLOOR)
    end
  end
  push!(datatmp,dl)
end

h=length(datatmp)
w=length(datatmp[1])

data = Matrix{Cell}(undef,h,w)
for i in 1:h
  data[i,:] = datatmp[i]
end

println("Day 11a: ",day11a(data))
println("Day 11b: ",day11b(data))
