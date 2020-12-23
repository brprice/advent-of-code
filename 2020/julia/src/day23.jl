# Do 100 rounds of a strange shuffle
function day23a(cups0)
  # Let's not worry about efficiency
  # Just keep the "current cup" in index 1
  # Then we don't need to worry about the next 3 cups we pick
  # up falling off the end of the array and wrapping around.
  cups = copy(cups0)
  minLabel, maxLabel = extrema(cups)
  for _ in 1:100
    pick = splice!(cups,2:4)
    dstidx = nothing
    dstSearch = cups[1]
    while dstidx === nothing
      dstSearch -= 1
      if dstSearch < minLabel
        dstSearch = maxLabel
      end
      dstidx = findnext(isequal(dstSearch),cups,1)
    end
    splice!(cups,dstidx+1:dstidx,pick)
    cups = circshift(cups,-1)
  end

  one = findnext(isequal(1),cups,1)
  others = cups[one+1:end]
  append!(others, cups[1:one-1])
  return join(map(string,others))
end

cups = map(x->parse(Int,x),split(chomp(read("../../data/day23",String)),""))
println(day23a(cups))
