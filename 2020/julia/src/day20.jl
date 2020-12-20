# Do a jigsaw puzzle
function day20a!(tiles)
  @assert isSimple(tiles) "non-simple puzzle?"
  # Since the puzzle is simple, we can just pick a first piece and grow the
  # puzzle from there, as there will be no ambiguity.
  puzzle = Dict{Tuple{Int,Int},Tuple{Int,Matrix{Bool}}}()
  # puzzle: a map from (x,y) location to (id,tileData-suitably-rotated)
  t1 = pop!(tiles)
  puzzle[0,0] = t1[1],t1[2]
  frontier = Set([(0,0)])

  dirs = [(-1,0),(1,0),(0,-1),(0,1)]
  rots = Dict(zip(dirs,[2,0,1,3]))

  while !isempty(frontier)
    f = pop!(frontier)
    for d in dirs
      fd = f .+ d
      if !in(fd, keys(puzzle))
        e = getedge(puzzle[f][2],d)
        t = popmatching!(reverse(e),tiles)
        if t !== nothing
          push!(frontier,fd)
          puzzle[fd] = t[1], rotr90(t[2],rots[d])
        end
      end
    end
  end

  # just assume the puzzle is square and complete
  fsts = map(x->x[1],collect(keys(puzzle)))
  snds = map(x->x[2],collect(keys(puzzle)))
  f1,f2 = extrema(fsts)
  s1,s2 = extrema(snds)
  return puzzle[f1,s1][1] * puzzle[f2,s1][1] * puzzle[f1,s2][1] * puzzle[f2,s2][1]

end

function getedge(tile,dir)
  # Returns the edge in "clockwise" orientation
  # i.e. read the top edge left-to-right
  # and the bottom edge right-to-left
  # (similarly for left and right edges)
  # so this operation commutes with rotation in the appropriate sense
  if dir == (-1,0)
    return tile[1,:]
  elseif dir == (1,0)
    return reverse(tile[end,:])
  elseif dir == (0,-1)
    return reverse(tile[:,1])
  elseif dir == (0,1)
    return tile[:,end]
  else
    error("bad direction")
  end
end

function popmatching!(edge,tiles)
  # pops a tile from 'tiles' with an edge matching 'edge'
  # the returned tile will be oriented and flipped so that
  # its upper edge (i.e. [1,:]) exactly matches 'edge'
  dirs = [(-1,0),(1,0),(0,-1),(0,1)]
  rots = Dict(zip(dirs,[0,2,1,3]))
  for it in tiles
    id = it[1]
    t = it[2]
    for d in dirs
      e = getedge(t,d)
      if e == edge
        delete!(tiles,id)
        return (id,rotr90(t,rots[d]))
      elseif reverse(e) == edge
        delete!(tiles,id)
        return (id,rotr90(permutedims(t),rots[(d[2],d[1])]))
      end
    end
  end
end

function isSimple(tiles)
  # We check that the puzzle is "simple" in the sense that
  # each edge has 0 or 1 matching pair (depending on if it is
  # a border piece or not. We also check that we have the
  # expected number of border edges for a square puzzle.
  n = length(tiles)
  sqrtN = Int(sqrt(n))
  @assert sqrtN^2==n "non-square puzzle?"

  edges = []
  for x in tiles
    i = x[1]
    t = x[2]
    append!(edges,[t[1,:],t[end,:],t[:,1],t[:,end]])
  end
  matches = map(edges) do e
    count(edges) do e2
      e == e2 || e == reverse(e2)
    end
  end

  # check for 1 and 2, rather than 0 and 1 as
  # each edge matches itself!
  hasPair = count(isequal(2),matches)
  noPair = count(isequal(1),matches)
  noPair == 4*sqrtN && hasPair == 4*n-noPair
end

tiles = Dict{Int,Matrix{Bool}}()
open("../../data/day20","r") do h
  while !eof(h)
  idS = readline(h)
  id = parse(Int,idS[6:end-1])
  tS = split(readuntil(h, "\n\n"),"\n")
  t = map(tS) do row
    map(isequal('#'),collect(row))
  end
  tiles[id] = hcat(t...)
  end
end

println(day20a!(tiles))
