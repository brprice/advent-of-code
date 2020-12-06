function day6a(groups)
  sum(g->length(union(Set(""),g...)),groups)
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
