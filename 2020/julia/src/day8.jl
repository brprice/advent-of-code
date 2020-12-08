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

# What is the value of accumulator immediately before an instruction is run for
# a second time
function day8a(code)
  seenIP = Set{Int}()
  program = Program(code)
  while !in(program.ip,seenIP)
    push!(seenIP,program.ip)
    step!(program)
  end
  return program.acc
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
