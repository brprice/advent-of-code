function day6a(groups)
  sum(g->length(union(Set(""),g...)),groups)
end

function day6b(groups)
  sum(g->length(intersect(Set(g[1]),g[2:end]...)),groups)
end

data = []
open("../../data/day6","r") do io
  while !eof(io)
    r=readuntil(io,"\n\n")
    rs=split(r,"\n",keepempty=false)
    push!(data,rs)
  end
end

println("Day 6a: ",day6a(data))
println("Day 6b: ",day6b(data))
