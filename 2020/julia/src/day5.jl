function seatId(s)
  # Julia has 'replace([1,2,3],1=>7,3=>4) == [7,2,4]', but the "multi-replace" does not work on strings...
  #replace(s, 'F'=>'0', 'B'=>'1', 'L'=>'0', 'R'=>'1')
  tr=Dict('F'=>'0', 'B'=>'1', 'L'=>'0', 'R'=>'1')
  ds=map(c->tr[c],s)
  parse(Int,"0b"*ds)
end

function day5a(data)
  maximum(seatId,data)
end

function day5b(data)
  seats=map(seatId,data)
  for i in seats
    if in(i+1,seats)
      continue
    end
    if in(i+2,seats)
      return i+1
    end
  end
end

data = readlines("../../data/day5")

print("Day 5a: ")
println(day5a(data))

print("Day 5b: ")
println(day5b(data))
