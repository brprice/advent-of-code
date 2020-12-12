function day12a(instrs)
  pos=(0,0)
  heading=(1,0)
  for i in instrs
    if i.op === N
      pos = pos .+ i.val .* (0,1)
    elseif i.op === S
      pos = pos .+ i.val .* (0,-1)
    elseif i.op === E
      pos = pos .+ i.val .* (1,0)
    elseif i.op === W
      pos = pos .+ i.val .* (-1,0)
    elseif i.op === L
      for i in 1:i.val
        heading = (-heading[2] , heading[1])
      end
    elseif i.op === R
      for i in 1:i.val
        heading = (heading[2] , -heading[1])
      end
    elseif i.op === F
      pos = pos .+ i.val .* heading
    end
  end
  return(abs(pos[1])+abs(pos[2]))
end

@enum Op N S E W L R F
struct Instr
  op :: Op
  val :: Int
end

# Convert R/L values from degrees to quater-turns
# i.e. divide by 90
data = map(readlines("../../data/day12")) do l
  os = l[1]
  v = parse(Int,l[2:end])
  i = findnext(x->x==os,"NSEWLRF",1)
  if i === nothing
    error("Unknown op: ",os)
  end
  op = Op(i-1)
  if op == L || op == R
    v /= 90
  end
  return Instr(op,v)
end

println("Day 12a: ",day12a(data))
