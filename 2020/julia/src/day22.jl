# Play a simple card game.
# Two players each have half a deck.
# Each round, draw the top card off each,
# the player with the highest card keeps both.
# (Assume cards are distinct, so ties don't happen.)
# Repeat until one deck is empty.
function day22a!(d1,d2)
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

data = readlines("../../data/day22")
i = findnext(isempty,data,1)
dP1 = data[1:i-1]
dP2 = data[i+1:end]
deckP1 = map(x->parse(Int,x),dP1[2:end])
deckP2 = map(x->parse(Int,x),dP2[2:end])

println(day22a!(deckP1,deckP2))
