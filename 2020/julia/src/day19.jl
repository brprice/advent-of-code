function day19a(rules,msgs)
  count(m->matches(m,rules,0),msgs)
end

# in part b, we change rules 8 and 11, so the grammar now has loops
function day19b(rules,msgs)
  rules[8] = [[42],[42,8]]
  rules[11] = [[42,31],[42,11,31]]
  count(m->matches(m,rules,0),msgs)
end

function matches(m,rules,i)
  any(match(m,rules,i)) do m
    isempty(m[2])
  end
end

function match(m,rules,i)
  r = rules[i]
  if typeof(r) == Char
    if !isempty(m) && m[1] == r
      return [(m[1]^1,m[2:end])]
    else
      return []
    end
  else
    alts = map(a -> matchlist(m,rules,a),r)
    vcat(alts...)
  end
end

function matchlist(m,rules,is)
  if isempty(is)
    return [("",m)]
  end
  ms = match(m,rules,is[1])
  ret = []
  for (matched,rest) in ms
    for (matched2,rest2) in matchlist(rest,rules,is[2:end])
      push!(ret,(matched*matched2,rest2))
    end
  end
  return ret
end

# We know that a rule is either of the form
# 1 : "a"  which is a rule numbered '1' and matches a literal character 'a'
# or
# 2 : 1 4 | 3 which is a rule numbered '2' and matches if either the rule 1
# matches a prefix, and the rest matches rule 4, or if rule 3 matches.
# We represent the rule 0: 1 2 | 3  as 0=>[[1,2],[3]]
rules = Dict{Int,Union{Char,Array{Array{Int,1},1}}}()
open("../../data/day19","r") do h
  rs = split(readuntil(h, "\n\n"),"\n")
  for r in rs
    name, rhs = split(r,": ")
    i = parse(Int,name)
    if rhs[1] == '"'
      global rules[i] = rhs[2]
    else
      alts = map(split(rhs," | ")) do a
        map(n->parse(Int,n),split(a," "))
      end
      global rules[i] = alts
    end
  end
  global msgs = readlines(h)
end

println(day19a(rules,msgs))
println(day19b(rules,msgs))
