# Do arithmetic, but + and * have the same precedence, left-to-right
function day18a(data)
  sum(evalA,data)
end

# Do arithmetic, but + has higher precedence than *
function day18b(data)
  sum(evalB,data)
end

function evalA(n :: Int)
  return n
end

function evalA(n :: AbstractString)
  return n
end

function evalA(arith)
  arith1 = map(evalA,arith)
  # now arith1 should be an alternating list of integers and
  # operations ("+" or "*"), of odd length
  if length(arith1) == 1
    return arith1[1]
  end
  # We don't bother about error handling, instead assuming that the
  # input is the right format
  ans = popfirst!(arith1)
  while !isempty(arith1)
    op = popfirst!(arith1)
    rhs = popfirst!(arith1)
    if op == "+"
      ans += rhs
    elseif op == "*"
      ans *= rhs
    end
  end
  return ans
end

function evalB(n :: Int)
  return n
end

function evalB(n :: AbstractString)
  return n
end

function evalB(arith)
  arith1 = map(evalB,arith)
  # now arith1 should be an alternating list of integers and
  # operations ("+" or "*"), of odd length
  if length(arith1) == 1
    return arith1[1]
  end
  # We don't bother about error handling, instead assuming that the
  # input is the right format
  arith2 = splitarray(arith1,"*")
  prod(ss->sum(filter(s->typeof(s)==Int,ss)),arith2)
end

# Why does the builtin "split" only work for strings?
function splitarray(xs,spl)
  splitted = []
  i = 1
  while i <= length(xs)
    j = findnext(isequal(spl),xs,i)
    if j === nothing
      j = length(xs)+1
    end
    if i<j
      push!(splitted,xs[i:j-1])
    end
    i = j+1
  end
  return splitted
end

function parse(s)
  s = replace(s,'('=>" ( ")
  s = replace(s,')'=>" ) ")
  t = split(s)
  parsed = parse_inner!(t)
  @assert isempty(t) "failed to parse fully"
  return parsed
end

# parses up to the first unmatched ')', if any, consuming it
# mutates its argument, removing the consumed elements
function parse_inner!(ts)
  ret = []
  while !isempty(ts)
    t = popfirst!(ts)
    if t == "*" || t == "+"
      push!(ret,t)
    elseif t == "("
      push!(ret,parse_inner!(ts))
    elseif t == ")"
      return ret
    else
      push!(ret,Base.parse(Int,t))
    end
  end
  return ret
end

data = map(parse,readlines("../../data/day18"))
println(day18a(data))
println(day18b(data))
