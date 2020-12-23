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

function shuffle(cups,rounds)
# This time, care about efficiency. Keep track of the label of the "current cup"
# and, for each label, which label is to its left, and which to its right.
# Assume that 'cups' is a permutation of 1:N for some N
  N = length(cups)
  cur = cups[1]
  left = similar(cups)
  right = similar(cups)
  prv = cups[end]
  for i in cups
    right[prv] = i
    left[i] = prv
    prv = i
  end

  for _ in 1:rounds
    a = right[cur]
    b = right[a]
    c = right[b]
    nxt = right[c]
    ins = cur
    while in(ins,[cur,a,b,c])
      ins -= 1
      if ins == 0
        ins = N
      end
    end
    insNxt = right[ins]

    right[cur] = nxt
    left[nxt] = cur
    right[ins] = a
    left[a] = ins
    right[c] = insNxt
    left[insNxt] = c
    cur = nxt
  end
return (left,right)
end

function day23aEfficient(cups)
  _, rs = shuffle(cups,100)
  ret = ""
  c = 1
  r = rs[c]
  while r != 1
    ret = ret*string(r)
    c = r
    r = rs[c]
  end
  return ret
end

function day23b(cups)
  _, rs = shuffle(cups,10000000)
  rs[1]*rs[rs[1]]
end


cups = map(x->parse(Int,x),split(chomp(read("../../data/day23",String)),""))
println(day23a(cups))
#@time println(day23aEfficient(cups)) # this is slightly quicker, but day23a is ~0.2s, so neither here or there really!
append!(cups,maximum(cups)+1:1000000)
println(day23b(cups))
