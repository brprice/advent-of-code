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

trees = readlines("../../data/day3")

print("Day 3a: ")
println(day3a(trees))
