# Move from "contains" to "may be contained in" relation
function invert(data)
  ret=Dict{String,Set{String}}()
  for (k,cts) in data
    for (_,c) in cts
      if !haskey(ret,c)
        ret[c]=Set()
      end
      push!(ret[c],k)
    end
  end
  return ret
end

# How many bags can eventually (in >=1 steps) contain a shiny gold bag?
function day7a(data)
  frontier=Set(["shiny gold"])
  found=Set{String}()
  edges=invert(data)
  while !isempty(frontier)
    new=get(edges,pop!(frontier),Set())
    union!(frontier,setdiff(new,found))
    union!(found,new)
  end
  return length(found)
end

# Read input as a dict. dict[k]=[(ni,bi)...] means
# bags of type k contain ni bags of type bi...
data = Dict{String,Array{Tuple{Int,String}}}()
open("../../data/day7","r") do io
  while !eof(io)
    r = readline(io)
    (big,littles) = split(r," bags contain ",keepempty=false)
    if littles == "no other bags."
      data[big]=[]
      continue
    end
    littles = split(littles[1:end-1],", ") # drop trailing period
    littles = map(littles) do l
      m=match(r"([[:digit:]]+) (.*) bags?",l)
      n=parse(Int,m.captures[1])
      b=m.captures[2]
      return (n,b)
    end
    data[big]=littles
  end
end

println("Day 7a: ",day7a(data))
