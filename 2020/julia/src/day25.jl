function day25a(cpk,dpk,base,mod)
  ce = discretelog(cpk,base,mod)
  return powermod(dpk,ce,mod)
end

# find k st a^k == ak (mod p)
function discretelog(ak,a,p)
# brute-force enumeration
  k = 0
  x = 1
  while x != ak
    k += 1
    x = (x*a) % p
  end
  return k
end

const mod = 20201227
const base = 7
cpk, dpk = map(x->parse(Int,x),readlines("../../data/day25"))

println(day25a(cpk,dpk,base,mod))
