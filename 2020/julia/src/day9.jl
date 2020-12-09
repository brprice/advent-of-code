# find the first number which is not the sum of two
# of the n numbers before it. These two numbers must
# have different values
function day9a(data,n)
# We consider each data[i] in turn, and keep track of
# (data[i-k],{data[i-k]+data[i-k+l] | l=1..n-1, two numbers differ}) for k=n..1
  cache = emptyCache()
  # preamble
  for i in data[1:n]
    insertCache!(cache,i)
  end
  # find first "bad" number
  for i in data[n+1:end]
    if !isInCache(cache,i)
      return i
    end
    dropOldestEntry!(cache)
    insertCache!(cache,i)
  end
  error("Didn't find a \"bad\" datapoint")
end

function day9aNaive(data,n)
  for i in n+1:length(data)
    isSum=false
    di=data[i]
    for j in 1:25
      if isSum
        break
      end
      dj=data[i-j]
      for k in 1:j-1
        dk=data[i-k]
        if dj != dk && dj+dk==di
          isSum=true
          break
        end
      end
    end
    if !isSum
      return di
    end
  end
  error("Didn't find a \"bad\" datapoint")
end

const Cache = Vector{Tuple{Int,Set{Int}}}
function emptyCache() :: Cache
  Cache()
end

function insertCache!(cache :: Cache,n :: Int)
  map(cache) do (m,sums)
    if m!=n
      push!(sums,m+n)
    end
  end
  push!(cache,(n,Set()))
end

function dropOldestEntry!(cache :: Cache)
  popfirst!(cache)
end

function isInCache(cache,i)
  any(ent -> in(i,ent[2]),cache)
end

data = map(x->parse(Int,x),readlines("../../data/day9"))

@time println("Day 9a (\"smart\" method): ",day9a(data,25))
@time println("Day 9a (naive method): ",day9aNaive(data,25))
