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

# Run a simple sequence of operations, with
# a strange bitmasking operation:
# each bit of the mask is '0','1' or 'X'
# '0' and '1' overwrites the corresponding bit of the written value
# whereas an 'X' leaves it alone.
# We represent such a mask '01X01' as
# mask.zeros = 0b10010
# mask.ones  = 0b01001
# Note that in reality everything is 36 bits long
# (values and addresses and mask)
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

function subsetOnes(n)
  pos = map(i->36-i,findall(c->c=='1',bitstring(n)[end-35:end]))
  ret = []
  for i in 0:1<<length(pos)-1
    x = 0
    for b in 0:length(pos)-1
      if (i>>b & 1) == 1
        x |= 1<<pos[1+b]
      end
    end
    push!(ret,x)
  end
  return ret
end

# Similar to part a, but this time the mask applies
# to the location, and
# '0': leave this bit unchanged
# '1': force this bit to be 1
# 'X': write to the two addresses corresponding to
# the two possible values of this bit
function day14b(data)
# We take a straightforward brute-force approach and literally implement the
# above description
  mem = Dict()
  mask = Mask(0,0)
  for i in data
    if typeof(i) == Mask
      mask = i
    elseif typeof(i) == Write
      base_loc = (i.loc & mask.zeros) | mask.ones
      floating = (1<<36-1) & (~(mask.zeros | mask.ones))
      for f in subsetOnes(floating)
        mem[base_loc | f] = i.val
      end
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
    zeros = mapreduce(i->1<<(36-i),|,findall(c->c=='0',rest),init=0)
    ones = mapreduce(i->1<<(36-i),|,findall(c->c=='1',rest),init=0)
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
println(day14b(data))
