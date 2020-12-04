# split on blank lines
# return an array of "passports" as strings, with a leading space, rather than
# actually parsing properly
# This enables an easy regex-based validator
function passports(data)
  pps=[]
  pp=""
  for l in data
    if isempty(l)
      push!(pps,pp)
      pp=""
    else
      pp = pp*" "*l
    end
  end
  push!(pps,pp)
  return pps
end

# We need a bunch of fields to be present
# Assume the input string has a leading space, and is a bunch of
# space-separated "key:value" pairs so we can search for (e.g.) " byr:" to find a
# key with no chance of finding a substring of a value
function isValid(passport)
  fields=["byr","iyr","eyr","hgt","hcl","ecl","pid"]
  all(f->occursin(Regex(" "*f*":"),passport),fields)
end

function day4a(data)
  count(isValid,passports(data))
end

data = readlines("../../data/day4")

print("Day 4a: ")
println(day4a(data))
