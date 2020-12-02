function parsePw(p)
  #format is "1-3 c: aaac" from which we extract (1,3,'c',"aaac")
  m=match(r"^([[:digit:]]+)-([[:digit:]]+) (.): ([[:alpha:]]*)$",p)
  (l,h,c,pw) = m.captures
  return (parse(Int,l),parse(Int,h),c[1],pw)
end

function isValid(l,h,c,pw)
  l <= count(x->(x==c),pw) <= h
end

function day2a(passwords)
  valid=0
  for p in passwords
    (l,h,c,pw) = parsePw(p)
    if isValid(l,h,c,pw)
      valid+=1
    end
  end
  return valid
end

function isValidB(l,h,c,pw)
  xor(pw[l] == c, pw[h] == c)
end

function day2b(passwords)
  valid=0
  for p in passwords
    (l,h,c,pw) = parsePw(p)
    if isValidB(l,h,c,pw)
      valid+=1
    end
  end
  return valid
end

passwords = readlines("../../data/day2")

print("Day 2a: ")
println(day2a(passwords))

print("Day 2b: ")
println(day2b(passwords))
