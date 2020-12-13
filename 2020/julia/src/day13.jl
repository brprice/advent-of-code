function day13a(time,ids0)
  ids = filter(x->x!=0,ids0)
  tis = map(i->(i-time%i,i),ids)
  t, i = minimum(tis)
  i*t
end

# we convert 'x' to 0
data = readlines("../../data/day13")
time = parse(Int,data[1])
ids = map(split(data[2],",")) do x
  if x == "x"
    return 0
  end
  parse(Int,x)
end

println("Day 13a: ",day13a(time,ids))
