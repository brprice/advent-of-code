# Play a simple card game.
# Two players each have half a deck.
# Each round, draw the top card off each,
# the player with the highest card keeps both.
# (Assume cards are distinct, so ties don't happen.)
# Repeat until one deck is empty.
function day22a!(d1,d2)
# This can infinite loop, e.g.
# d1: 43, 19
# d2: 2, 29, 14
# but we will assume that our puzzle input avoids this!
# (We explicitly have to detect this in part b, and bail out.)
  while !(isempty(d1) || isempty(d2))
    c1 = popfirst!(d1)
    c2 = popfirst!(d2)
    if c1 > c2
      append!(d1,[c1,c2])
    else
      append!(d2,[c2,c1])
    end
  end

  winner = if isempty(d1); d2 ; else d1; end

  # We are asked to find total points of the winner
  # where if they have a deck of N cards, the top
  # card is worth N times its face value,
  # the next card is N-1 times its face value
  # etc, with the bottom card being 1 times its face value
  return cumsum(cumsum(winner))[end]
end

# Similar to part a, but with a "recursive game"
function day22b!(d1,d2)
  _, winningDeck = playRecursive!(d1,d2)
  return cumsum(cumsum(winningDeck))[end]
end

function playRecursive!(d1,d2)
# return a pair of the winner and their ending deck
  seen = Set()
  while !(isempty(d1) || isempty(d2))
    # If we have been here before, abort and player 1 wins by default
    if in((d1,d2),seen)
      return (1,d1)
    end
    push!(seen,(copy(d1),copy(d2)))

    c1 = popfirst!(d1)
    c2 = popfirst!(d2)

    if length(d1) >= c1 && length(d2) >= c2
      roundWinner, _ = playRecursive!(d1[1:c1],d2[1:c2])
    else
      if c1 > c2
        roundWinner = 1
      else
        roundWinner = 2
      end
    end
    if roundWinner == 1
      append!(d1,[c1,c2])
    else
      append!(d2,[c2,c1])
    end
  end
  if isempty(d1)
    return (2,d2)
  else
    return (1,d1)
  end
end

data = readlines("../../data/day22")
i = findnext(isempty,data,1)
dP1 = data[1:i-1]
dP2 = data[i+1:end]
deckP1 = map(x->parse(Int,x),dP1[2:end])
deckP2 = map(x->parse(Int,x),dP2[2:end])

println(day22a!(copy(deckP1),copy(deckP2)))
println(day22b!(deckP1,deckP2))
