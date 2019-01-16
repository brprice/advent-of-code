-- Day 19: simulate a simple cpu
-- Like day 16, but more registers, one of which is bound to IP
-- 6 registers: 0..5, seem to contain small integers (not specified in problem)
-- 16 instructions

import Data.Bits ((.|.),(.&.))
import qualified Data.Vector as V
import Data.Void

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Util

data Reg = R0 | R1 | R2 | R3 | R4 | R5 deriving Show
data StateR = StateR Int Int Int Int Int Int deriving (Show, Eq)
data State = State Int StateR -- IP & registers

get :: StateR -> Reg -> Int
get (StateR r0 _ _ _ _ _) R0 = r0
get (StateR _ r1 _ _ _ _) R1 = r1
get (StateR _ _ r2 _ _ _) R2 = r2
get (StateR _ _ _ r3 _ _) R3 = r3
get (StateR _ _ _ _ r4 _) R4 = r4
get (StateR _ _ _ _ _ r5) R5 = r5

set :: StateR -> Reg -> Int -> StateR
set (StateR _ r1 r2 r3 r4 r5) R0 n = StateR n r1 r2 r3 r4 r5
set (StateR r0 _ r2 r3 r4 r5) R1 n = StateR r0 n r2 r3 r4 r5
set (StateR r0 r1 _ r3 r4 r5) R2 n = StateR r0 r1 n r3 r4 r5
set (StateR r0 r1 r2 _ r4 r5) R3 n = StateR r0 r1 r2 n r4 r5
set (StateR r0 r1 r2 r3 _ r5) R4 n = StateR r0 r1 r2 r3 n r5
set (StateR r0 r1 r2 r3 r4 _) R5 n = StateR r0 r1 r2 r3 r4 n

setBit :: StateR -> Reg -> Bool -> StateR
setBit s r True = set s r 1
setBit s r False = set s r 0

data Op = AddR Reg Reg Reg
        | AddI Reg Int Reg
        | MulR Reg Reg Reg
        | MulI Reg Int Reg
        | BAnR Reg Reg Reg
        | BAnI Reg Int Reg
        | BOrR Reg Reg Reg
        | BOrI Reg Int Reg
        | SetR Reg Reg -- NB: middle operand ignored
        | SetI Int Reg -- NB: middle operand ignored
        | GtIR Int Reg Reg
        | GtRI Reg Int Reg
        | GtRR Reg Reg Reg
        | EqIR Int Reg Reg
        | EqRI Reg Int Reg
        | EqRR Reg Reg Reg
  deriving Show

doArithR :: (Int -> Int -> Int) -> Reg -> Reg -> Reg -> StateR -> StateR
doArithR f ra rb rc s =  set s rc $ f (get s ra) (get s rb)

doArithI :: (Int -> Int -> Int) -> Reg -> Int -> Reg -> StateR -> StateR
doArithI f ra i rc s =  set s rc $ f (get s ra) i


doOp :: Op -> StateR -> StateR
doOp (AddR ra rb rc) s = doArithR (+) ra rb rc s
doOp (AddI ra i rc) s = doArithI (+) ra i rc s
doOp (MulR ra rb rc) s = doArithR (*) ra rb rc s
doOp (MulI ra i rc) s = doArithI (*) ra i rc s
doOp (BAnR ra rb rc) s = doArithR (.&.) ra rb rc s
doOp (BAnI ra i rc) s = doArithI (.&.) ra i rc s
doOp (BOrR ra rb rc) s = doArithR (.|.) ra rb rc s
doOp (BOrI ra i rc) s = doArithI (.|.) ra i rc s
doOp (SetR ra rc) s = set s rc $ get s ra
doOp (SetI i rc) s = set s rc i
doOp (GtIR i rb rc) s = setBit s rc $ i > get s rb
doOp (GtRI ra i rc) s = setBit s rc $ get s ra > i
doOp (GtRR ra rb rc) s = setBit s rc $ get s ra > get s rb
doOp (EqIR i rb rc) s = setBit s rc $ i == get s rb
doOp (EqRI ra i rc) s = setBit s rc $ get s ra == i
doOp (EqRR ra rb rc) s = setBit s rc $ get s ra == get s rb

-- 19a: Run a program, what's left in R0?
-- Let's assume start off in "all registers are 0"

day19a_parser :: Parser (Reg, V.Vector Op)
day19a_parser = (,)
            <$> bind <* eol
            <*> program
  where bind = string "#ip " *> reg
        program = V.fromList <$> many (inst <* eol)
        reg = R0 <$ string "0"
          <|> R1 <$ string "1"
          <|> R2 <$ string "2"
          <|> R3 <$ string "3"
          <|> R4 <$ string "4"
          <|> R5 <$ string "5"
        inst = instRRR <|> instRIR <|> instR_R <|> instI_R <|> instIRR
        instRRR = opRRR <* string " " <*> reg <* string " " <*> reg <* string " " <*> reg
        instRIR = opRIR <* string " " <*> reg <* string " " <*> decimal <* string " " <*> reg
        instR_R = opR_R <* string " " <*> reg <* string " " <* (decimal :: Parser Int) <* string " " <*> reg
        instI_R = opI_R <* string " " <*> decimal <* string " " <* (decimal :: Parser Int) <* string " " <*> reg
        instIRR = opIRR <* string " " <*> decimal <* string " " <*> reg <* string " " <*> reg
        opRRR = AddR <$ string "addr"
            <|> MulR <$ string "mulr"
            <|> BAnR <$ string "banr"
            <|> BOrR <$ string "borr"
            <|> GtRR <$ string "gtrr"
            <|> EqRR <$ string "eqrr"
        opRIR = AddI <$ string "addi"
            <|> MulI <$ string "muli"
            <|> BAnI <$ string "bani"
            <|> BOrI <$ string "bori"
            <|> GtRI <$ string "gtri"
            <|> EqRI <$ string "eqri"
        opR_R = SetR <$ string "setr"
        opI_R = SetI <$ string "seti"
        opIRR = GtIR <$ string "gtir"
            <|> EqIR <$ string "eqir"

runProg :: Reg -> V.Vector Op -> State -> StateR
runProg ipr prog = go
  where go (State ip regs) = case prog V.!? ip of
                               Nothing -> regs -- halt
                               Just inst -> let regs' = doOp inst  $ set regs ipr ip
                                                ip' = 1 + get regs' ipr
                                            in go (State ip' regs')

day19a_solve :: (Reg, V.Vector Op) -> Either Void Int
day19a_solve (ipr, prog) = pure $ flip get R0 $ runProg ipr prog (State 0 (StateR 0 0 0 0 0 0))

day19a_main :: IO ()
day19a_main = generic_main "../data/19a" day19a_parser day19a_solve show



-- 19b: same as before, but starts with R0 = 1
-- THIS NAIVE IMPLEMENTATION IS FAR TOO SLOW
{-
day19b_solve :: (Reg, V.Vector Op) -> Either Void Int
day19b_solve (ipr, prog) = pure $ flip get R0 $ runProg ipr prog (State 0 (StateR 1 0 0 0 0 0))

day19b_main :: IO ()
day19b_main = generic_main "../data/19a" day19a_parser day19b_solve show
-}

-- LET'S STUDY THE CODE

{-
My Code:
#ip 2
00 : addi 2 16 2
01 : seti 1 4 3
02 : seti 1 5 1
03 : mulr 3 1 5
04 : eqrr 5 4 5
05 : addr 5 2 2
06 : addi 2 1 2
07 : addr 3 0 0
08 : addi 1 1 1
09 : gtrr 1 4 5
10 : addr 2 5 2
11 : seti 2 9 2
12 : addi 3 1 3
13 : gtrr 3 4 5
14 : addr 5 2 2
15 : seti 1 6 2
16 : mulr 2 2 2
17 : addi 4 2 4
18 : mulr 4 4 4
19 : mulr 2 4 4
20 : muli 4 11 4
21 : addi 5 7 5
22 : mulr 5 2 5
23 : addi 5 4 5
24 : addr 4 5 4
25 : addr 2 0 2
26 : seti 0 1 2
27 : setr 2 1 5
28 : mulr 5 2 5
29 : addr 2 5 5
30 : mulr 2 5 5
31 : muli 5 14 5
32 : mulr 5 2 5
33 : addr 4 5 4
34 : seti 0 6 0
35 : seti 0 6 2

-- Let's reverse engineer it!
-- step 1 : convert to readable syntax
00 : IP += 16
01 : R3 <- 1
02 : R1 <- 1
03 : R5 = R3 * R1
04 : R5 = R5==R4  -- True = 1
05 : IP += R5
06 : IP += 1
07 : R0 += R3
08 : R1 += 1
09 : R5 = R1 > R4
10 : IP += R5
11 : IP = 2
12 : R3 += 1
13 : R5 = R3 > R4
14 : IP += R5
15 : IP = 1
16 : IP *= IP
17 : R4 += 2
18 : R4 *= R4
19 : R4 *= IP
20 : R4 *= 11
21 : R5 += 7
22 : R5 *= IP
23 : R5 += 4
24 : R4 += R5
25 : IP += R0
26 : IP = 0
27 : R5 = IP
28 : R5 *= IP
29 : R5 += IP
30 : R5 *= IP
31 : R5 *= 14
32 : R5 *= IP
33 : R4 += R5
34 : R0 = 0
35 : IP = 0

-- Step 2: inline IP, annotate known jump targets
00 : IP = 16         -> Jump to 17
01 : R3 <- 1         * From 26, 35
02 : R1 <- 1         * From 15
03 : R5 = R3 * R1    * From 11
04 : R5 = R5==R4  -- True = 1
05 : IP = R5 + 5     -> Jump to R5 + 6 (either 06 or 07, unless we jump to this line)
06 : IP = 7          -> Jump to 08  * From 05 (conditionally) ^^^
07 : R0 += R3                       * From 05 (conditionally) ^^^
08 : R1 += 1         * From 06
09 : R5 = R1 > R4
10 : IP = 10 + R5    -> Jump to R5 + 11 (either 11 or 12, unless we jump to this line)
11 : IP = 2          -> Jump to 03  * From 10 (conditionally) ^^^
12 : R3 += 1                        * From 10 (conditionally) ^^^
13 : R5 = R3 > R4
14 : IP = 14 + R5    -> Jump to R5 + 15 (either 15 or 16, unless we jump to this line)
15 : IP = 1          -> Jump to 02 * From 14 (conditionally) ^^^
16 : IP = 256        -> HALT       * From 14 (conditionally) ^^^
17 : R4 += 2          * From 00
18 : R4 *= R4
19 : R4 *= 19
20 : R4 *= 11
21 : R5 += 7
22 : R5 *= 22
23 : R5 += 4
24 : R4 += R5
25 : IP = 25 + R0    -> Jump to R0 + 26 (WHAT IS R0?)
26 : IP = 0          -> Jump to 01
27 : R5 = 27
28 : R5 *= 28
29 : R5 += 29
30 : R5 *= 30
31 : R5 *= 14
32 : R5 *= 32
33 : R4 += R5
34 : R0 = 0
35 : IP = 0          -> Jump to 01

-- Step 2.5: Flow analysis / work out what R0 is:
We flow 00,17..25
Then either 26,01 or 27..35,01
Then stuck in loops staying in lines 01..16

-- Step 3: Higher level (assuming R0 = 0 or R0 = 1)
R4 = (R4+2)^2*19*11
R5 = (R5+7)*22+4
R4 = R4 + R5
if(R0 = 1) {
  R5 = (27*28+29)*30*14*32
  R4 = R4 + R5
  R0 = 0
}
-- now know R0 = 0
R3 = 1

L1:
R1 = 1
L2:
R5 = R3*R1
if(R5 == R4) {
  R0 = R0 + R3
}
R1 = R1 + 1
if(R1 > R4) {
  R3 = R3 + 1
  if (R3 > R4) {
    HALT
  } else {
    goto L1
  }
} else {
  goto L2
}


-- Step 4: Make it loopy (assuming R0 = 0 or R0 = 1)
R4 = (R4+2)^2*19*11
R5 = (R5+7)*22+4
R4 = R4 + R5
if(R0 = 1) {
  R5 = (27*28+29)*30*14*32
  R4 = R4 + R5
  R0 = 0
}
-- now know R0 = 0
R3 = 1
R1 = 1

for (R3 = 1, R3<=R4, R3++) {
  for (R1 = 1, R1<=R4, R1++) {
    if (R3*R1 == R4) {
      R0 += R3;
    }
  }
}


-- Step 5: Make it mathy (still assuming R0 = 0 or R0 = 1)
R4 = (R4+2)^2*209
R4 = R4 + (R5+7)*22+4
R4 = R4 + R0 * 10550400
R0 = sum-distinct-factors-of(R4)

-- Step 6: inline (still assuming R0 = 0 or R0 = 1)
n = (R4+2)^2*209 + (R5+7)*22+4 + R0*10550400
R0 = sum-distinct-factors-of(n)

-- Step 7: specialise to R4 = R5 = 0 initially (still assuming R0 = 0 or R0 = 1)
n = 994 + R0*10550400
R0 = sum-distinct-factors-of(n)

-}

-- OK, can now easilly implement the underlying algorithm

-- sum the distinct factors (poor implementation, but good enough for this problem)
reverseEngineered :: Integer -> Integer
reverseEngineered n = sum $ [m | m<-[1..n], n`mod`m==0]

day19b_main :: IO ()
day19b_main = print $ reverseEngineered $ 994 + 10550400
