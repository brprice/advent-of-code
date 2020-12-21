# Given an input record ([a,b,c],[X,Y]), we know that
# the allergen X is contained in one of the ingredients
# a, b or c. We do NOT know that allergen Z is not in a, b or c.
# We also know that allergens are in exactly one ingredient
# (across all records), and each ingredient has 0 or 1 allergen.
# Find which ingredients contain which allergens.
# We return (d,s) where d is a dict mapping allergens to ingredient
# and s is a set of safe ingredients
function findAllergens(data)
# Strategy: from (is1,as1) and (is2,as2) with X in as1 and X in as2
# we know that X is contained in one of the ingredients in the
# intersection of is1 and is2.
# We hope that this will be a singleton for at least one allergen,
# and thus identify which ingredient it is in. We can then forget
# about that pair and repeat.
  ingredients = Set()
  unknown = Dict()
  for (is,as) in data
    union!(ingredients,is)
    for a in as
      x = get(unknown,a,nothing)
      if x === nothing
        unknown[a] = Set(is)
      else
        intersect!(unknown[a],is)
      end
    end
  end

  known = Dict()
  while !isempty(unknown)
    progress = false
    for (a,is) in unknown
      if length(is) == 0
        error("impossible allergen: ",a)
      elseif length(is) == 1
        i = pop!(is)
        known[a] = i
        delete!(unknown,a)
        delete!(ingredients,i)
        for (_,x) in unknown
          delete!(x,i)
        end
        progress = true
        break
      end
    end
    if progress == false
      error("got stuck at:\n  known: ",known,"\n  unknown: ",unknown)
    end
  end

  return (known,ingredients)
end

# How many times do allergen-free ingredients appear?
function day21a(data,safe)
  total = 0
  for (is,_) in data
    total += length(intersect(is,safe))
  end
  return total
end

# What are the dangerous ingredients?
function day21b(allergens)
  join(map(x->x[2],sort(collect(pairs(allergens)))),',')
end

data = map(readlines("../../data/day21")) do l
  ingredients0, allergens0 = split(l,"(")
  ingredients = split(ingredients0)
  allergens = split(allergens0[10:end-1],", ") # we drop the "contains " prefix and the closing paren
  ingredients, allergens
end

allergens, safe = findAllergens(data)
println(day21a(data,safe))
println(day21b(allergens))
