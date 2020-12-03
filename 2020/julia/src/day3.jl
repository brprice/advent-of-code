# Count trees '#' at positions (i,3*i (mod w)); 1-indexed arrays
function day3a(trees)
  w=length(trees[1]) # assume non-empty map which is square
  hit=0
  x=1
  for row in trees
    if row[x]=='#'
      hit += 1
    end
    x += 3
    x = mod1(x,w)
  end
  return hit
end

# As day3a, but for general steps: r right and d down
# Thus r=3, d=1 should match with day3a
# We assume r>=0, d>0
function traverseSlope(trees,(r,d))
  w=length(trees[1]) # assume non-empty map which is square
  h=length(trees)
  hit=0
  x=1
  y=1
  while y<=h
    if trees[y][x]=='#'
      hit += 1
    end
    y += d
    x += r
    x = mod1(x,w)
  end
  return hit
end

function day3b(trees)
  slopes=[(1,1),(3,1),(5,1),(7,1),(1,2)]
  z = map(s -> traverseSlope(trees,s),slopes)
  prod(z)
end


trees = readlines("../../data/day3")

print("Day 3a: ")
println(day3a(trees))

print("Day 3b: ")
println(day3b(trees))
