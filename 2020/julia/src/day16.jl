function day16a(rules,tickets)
# Simple-minded brute force search
# We could compact ranges across rules
# (i.e. change ruleA:1-5 , ruleB:3-9 into just 1-9)
# or create a bitvector of "good according to at least one rule"
# to speed up calculation of whether each number is valid
# However, this is entirely unnecessary for this problem
  invalid = 0
  for t in tickets
    for n in t
      if all(r->!isValid(r,n),rules)
        invalid += n
      end
    end
  end
  return invalid
end

function isValid((field,rules),n)
  any(rng->in(n,rng),rules)
end

rules = []
yourTicket = []
otherTickets = []
open("../../data/day16","r") do file
  r = readline(file)
  while r != ""
    f,rs0 = split(r,": ")
    rs = map(split(rs0," or ")) do rng
      l,h = map(x->parse(Int,x),split(rng,"-"))
      l:h
    end
    push!(rules,(f,rs))
    r = readline(file)
  end

  r = readline(file)
  expect = "your ticket:"
  @assert r==expect ("Unexpected format, wanted '" * expect * "'")
  r = readline(file)
  append!(yourTicket,map(x->parse(Int,x),split(r,",")))

  r = readline(file)
  expect = ""
  @assert r==expect ("Unexpected format, wanted '" * expect * "'")
  r = readline(file)
  expect = "nearby tickets:"
  @assert r==expect ("Unexpected format, wanted '" * expect * "'")
  r = readline(file)
  while r != ""
    push!(otherTickets,map(x->parse(Int,x),split(r,",")))
    r = readline(file)
  end
end

println(day16a(rules,otherTickets))
