# basically game of life
function day11a(data)
  (h,w)=size(data)
  while true
    next = step(data)
    if next==data
      break
    end
    data=next
  end
  count(x->x==OCCUPIED,data)
end

function step(data)
  (h,w)=size(data)
  next = similar(data)
  for i in 1:h, j in 1:w
    if data[i,j] == FLOOR
      next[i,j] = FLOOR
      continue
    end
    nbd=getNbd(data,i,j)
    o=count(x->x==OCCUPIED,nbd)
    if data[i,j] == EMPTY && o == 0
      next[i,j] = OCCUPIED
    elseif data[i,j] == OCCUPIED && o >= 4
      next[i,j] = EMPTY
    else
      next[i,j] = data[i,j]
    end
  end
  next
end

function getNbd(data,i,j)
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
