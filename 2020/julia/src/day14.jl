struct Write
  loc :: Int
  val :: Int
end

struct Mask
  # bitmask: '1' shows "set", both for zeros and ones
  # i.e. to mask you do (x | ones) & (~ zeros)
  zeros :: Int
  ones :: Int
end

function day14a(data)
  mem = Dict()
  mask = Mask(0,0)
  for i in data
    if typeof(i) == Mask
      mask = i
    elseif typeof(i) == Write
      mem[i.loc] = (i.val | mask.ones) & (~ mask.zeros)
    else
      error("bad instruction")
    end
  end
  sum(values(mem))
end

data = map(readlines("../../data/day14")) do l
  op = l[1:4]
  if op == "mask"
    rest = l[8:end]
    zeros = mapreduce(i->1<<(36-i),|,findall(c->c=='0',rest))
    ones = mapreduce(i->1<<(36-i),|,findall(c->c=='1',rest))
    Mask(zeros,ones)
  elseif op == "mem["
    rest = l[5:end]
    loc,val = split(rest,"] = ")
    Write(parse(Int,loc),parse(Int,val))
  else
    error("cannot parse")
  end
end

println(day14a(data))
