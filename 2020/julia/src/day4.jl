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

function matchNumRange(r,l,h,s)
  m = match(r,s)
  if m === nothing
    return false
  end
  l <= parse(Int,m.captures[1]) <= h
end

# Now we have some validation criteria for each field
function isValidB(passport)
  byr=matchNumRange(r"(?: |^)byr:(\d{4})( |$)",1920,2002,passport)
  iyr=matchNumRange(r"(?: |^)iyr:(\d{4})( |$)",2010,2020,passport)
  eyr=matchNumRange(r"(?: |^)eyr:(\d{4})( |$)",2020,2030,passport)
  hgtCm=matchNumRange(r"(?: |^)hgt:(\d*)cm( |$)",150,193,passport)
  hgtIn=matchNumRange(r"(?: |^)hgt:(\d*)in( |$)",59,76,passport)
  hcl=occursin(r"( |^)hcl:#[[:xdigit:]]{6}( |$)",passport)
  ecl=occursin(r"( |^)ecl:(amb|blu|brn|gry|grn|hzl|oth)( |$)",passport)
  pid=occursin(r"( |^)pid:\d{9}( |$)",passport)

  byr && iyr && eyr && (hgtCm || hgtIn) && hcl && ecl && pid
end

function day4b(data)
  count(isValidB,passports(data))
end


data = readlines("../../data/day4")

print("Day 4a: ")
println(day4a(data))

print("Day 4b: ")
println(day4b(data))
