@enum OpCode ACC JMP NOP

struct Instr
  op :: OpCode
  arg :: Int
end

mutable struct Program
  code :: Array{Instr}
  ip :: Int # 1-indexed, as Julia has 1-indexed arrays
  acc :: Int
end

function Program(code)
  Program(code,1,0)
end

function step!(program)
  op=program.code[program.ip]
  opcode=op.op
  if opcode === ACC
    program.acc += op.arg
    program.ip += 1
  elseif opcode === JMP
    program.ip += op.arg
  elseif opcode === NOP
    program.ip += 1
  else
    error("unknown opcode: $opcode")
  end
  return program
end

@enum RunResult Terminate Loop
function run(program)
  seenIP = Set{Int}()
  while !in(program.ip,seenIP) && program.ip <= length(program.code)
    push!(seenIP,program.ip)
    step!(program)
  end
  reason = program.ip <= length(program.code) ? Loop : Terminate
  return (reason,program)
end

# What is the value of accumulator immediately before an instruction is run for
# a second time
function day8a(code)
  (_,program) = run(Program(code)) # assume it loops, as today's question claims
  return program.acc
end

# Change exactly one nop to a jmp or vice versa so it terminates (by running off
# the end of the program).
# What is the accumulator when it terminates?
function day8b(code)
  # Nothing fancy here, just brute force. Still plenty fast enough
  for (i,ins) in enumerate(code)
    if ins.op === NOP || ins.op === JMP
      newcode = copy(code)
      newcode[i] = Instr(ins.op===NOP ? JMP : NOP, ins.arg)
      (reason,program)=run(Program(newcode))
      if reason === Terminate
        return program.acc
      end
    end
  end
end

program=map(readlines("../../data/day8")) do l
  (opS,argS)=split(l," ")
  arg=parse(Int,argS)
  if opS=="acc"
    op = ACC
  elseif opS=="jmp"
    op = JMP
  elseif opS=="nop"
    op = NOP
  else
    error("unknown op: $opS")
  end
  return Instr(op,arg)
end

println("Day 8a: ",day8a(program))
println("Day 8b: ",day8b(program))
