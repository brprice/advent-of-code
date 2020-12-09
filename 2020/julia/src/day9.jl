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

# part b: run part a to get a number 'key' now find a contiguous set (at least 2)
# from the data which sum to 'key'. Answer is the sum of the smallest and largest
# number in this range
function day9b(data,n)
  key = day9a(data,n)
# For each possible start i, consider data[i:i+j] for j=1,2,...
# until we have a sum which is too big, at which point try the next i
  for i in 1:length(data)
    sum = data[i]
    for j in i+1:length(data)
      sum += data[j]
      if sum == key
        vals = data[i:j]
        (mini,maxi) = extrema(vals)
        return mini+maxi
      elseif sum > key
        break
      end
    end
  end
  error("Didn't find consecutive data summing to the key")
end

function day9bNaive(data,n)
  key = day9a(data,n) # keep this the same as in day9b for fair comparison
# For each possible start i, consider data[i:i+j] for j=1,2,...
# until we have a sum which is too big, at which point try the next i
  for i in 1:length(data)
    for j in i+1:length(data)
      vals = data[i:j]
      if sum(vals) == key
        (mini,maxi) = extrema(vals)
        return mini+maxi
      end
      # For benchmarking: these three lines perform the early cutoff optimisation
      # with these in, we are essentially the same as day9b, but repeatedly indexing the vector
      # (and only slightly slower); with these commented out we do a lot of extra work and are
      # much slower (but only ~.2s compared to ~.02s)
      # we copy around far too much data in either case, giving ~13% or ~15% GC time!
      #if sum(vals) > key
      #  break
      #end
    end
  end
  error("Didn't find consecutive data summing to the key")
end

data = map(x->parse(Int,x),readlines("../../data/day9"))
# For part b, I assume all numbers are positive
for i in data
  if i<0
    error("You have a negative number, abort!")
  end
end

# It turns out that for part a, the naive method was faster!
@time println("Day 9a (\"smart\" method): ",day9a(data,25))
@time println("Day 9a (naive method): ",day9aNaive(data,25))
# It turns out that for part b, the naive method was slower
# mostly due to not detecting that the range under consideration
# was already too large and realising that growing it is pointless
# but also a bit due to repeatedly fetching the same data from the array
@time println("Day 9b (smart method): ",day9b(data,25))
@time println("Day 9b (naive method): ",day9bNaive(data,25))
