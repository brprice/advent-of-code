import Data.Bits
import qualified Data.Set as S

{- This is a reverse-engineer-the-elf-code challenge

--Step 0: the code
#ip 4
00 : seti 123 0 3
01 : bani 3 456 3
02 : eqri 3 72 3
03 : addr 3 4 4
04 : seti 0 0 4
05 : seti 0 2 3
06 : bori 3 65536 2
07 : seti 1397714 1 3
08 : bani 2 255 5
09 : addr 3 5 3
10 : bani 3 16777215 3
11 : muli 3 65899 3
12 : bani 3 16777215 3
13 : gtir 256 2 5
14 : addr 5 4 4
15 : addi 4 1 4
16 : seti 27 6 4
17 : seti 0 6 5
18 : addi 5 1 1
19 : muli 1 256 1
20 : gtrr 1 2 1
21 : addr 1 4 4
22 : addi 4 1 4
23 : seti 25 2 4
24 : addi 5 1 5
25 : seti 17 0 4
26 : setr 5 7 2
27 : seti 7 4 4
28 : eqrr 3 0 5
29 : addr 5 4 4
30 : seti 5 8 4

-- Step 1: make it readable, and inline IP
00 : R3 = 123
01 : R3 = R3 & 456
02 : R3 = R3 == 72 -- True ~> 1
03 : IP = R3 + 03
04 : IP = 0
05 : R3 = 0
06 : R2 = R3 | 65536
07 : R3 = 1397714
08 : R5 = R2 & 255
09 : R3 = R3 + R5
10 : R3 = R3 & 16777215
11 : R3 = R3 * 65899
12 : R3 = R3 & 16777215
13 : R5 = 256 > R2 -- True ~> 1
14 : IP = R5 + 14
15 : IP = 16
16 : IP = 27
17 : R5 = 0
18 : R1 = R5 + 1
19 : R1 = R1 * 256
20 : R1 = R1 > R2 -- True ~> 1
21 : IP = R1 + 21
22 : IP = 23
23 : IP = 25
24 : R5 = R5 + 1
25 : IP = 17
26 : R2 = R5
27 : IP = 7
28 : R5 = R3 == R0 -- True ~> 1
29 : IP = R5 + 29
30 : IP = 5


-- Step 2: higher level
-- Lines 00-05 are only executed once, and have no affect

L1 : R2 = R3 | 65536
     R3 = 1397714
L2 : R5 = R2 & 255
     R3 = R3 + R5
     R3 = R3 & 16777215
     R3 = R3 * 65899
     R3 = R3 & 16777215
     R5 = 256 > R2 -- True ~> 1
     if (256 > R2) then goto L7 else goto L3
L3 : R5 = 0
L4 : R1 = R5 + 1
     R1 = R1 * 256
     R1 = R1 > R2 -- True ~> 1
     if (R1 > R2) then goto L6 else goto L5
L5 : R5 = R5 + 1
     goto L4
L6 : R2 = R5
     goto L2
L7 : R5 = R3 == R0 -- True ~> 1
     if (R3 == R0) then HALT else goto L1


-- Step 3: continue, noting that R5 and R1 is always set just before use

L1 : R2 = R3 | 65536
     R3 = 1397714
L2 : R5 = R2 & 255
     R3 = R3 + R5
     R3 = R3 & 16777215
     R3 = R3 * 65899
     R3 = R3 & 16777215
     if (256 > R2)
       then if (R3 == R0)
              then HALT
              else goto L1
       else goto L3

L3 : R5 = 0
L4 : if ((R5+1)*256 > R2)
       then R2 = R5
            goto L2
       else R5 = R5 + 1
            goto L4


-- Step 4: continue, make loopy

     R2 = R3 | 65536
     R3 = 1397714

L2 : R5 = R2 & 255
     R3 = R3 + R5
     R3 = R3 & 16777215
     R3 = R3 * 65899
     R3 = R3 & 16777215
     if (256 > R2)
       then if (R3 == R0)
              then HALT
              else R2 = R3 | 65536
                   R3 = 1397714
       else R5 = 0
            while ((R5+1)*256 <= R2)
              R5++
            R2 = R5
     goto L2


-- Step 5: continue

     R2 = R3 | 65536
     R3 = 1397714

     R5 = R2 & 255
     R3 = R3 + R5
     R3 = R3 & 16777215
     R3 = R3 * 65899
     R3 = R3 & 16777215

     while ! (256 > R2 && R3 == R0)
       if (256 > R2)
       then R2 = R3 | 65536
            R3 = 1397714
       else R5 = 0
            while ((R5+1)*256 <= R2)
              R5++
            R2 = R5
       R5 = R2 & 255
       R3 = R3 + R5
       R3 = R3 & 16777215
       R3 = R3 * 65899
       R3 = R3 & 16777215

-- Step 6: evaluate arithmetic

     R2 = 65536
     R3 = 1039046

     while ! (256 > R2 && R3 == R0)
       if (256 > R2)
       then R2 = R3 | 65536
            R3 = 1397714
       else R2 = R2/256
       R3 = R3 + (R2 & 255)
       R3 = R3 & 16777215
       R3 = R3 * 65899
       R3 = R3 & 16777215

-}

-- Let's assume that fewest instructions is same as least times around this loop
day21a_sequence :: [(Integer,Integer)]
day21a_sequence = iterate go (65536,1039046)
  where go (r2,r3) = if 256 > r2
                     then let r2' = r3 .|. 65536
                              r3' = foo r2' 1397714
                          in (r2',r3')
                     else let r2' = r2`div`256
                          in (r2', foo r2' r3)
        foo r2 r3 = let a = r3 + (r2 .&. 255)
                        b = a .&. 16777215
                        c = b * 65899
                        d = c .&. 16777215
                    in d

couldTerminate :: (Integer,Integer) -> Bool
couldTerminate = (<256).fst

-- 21a: what is the value of R0 that will terminate the loop the quickest
day21a_solve :: Integer
day21a_solve = snd $ head $ filter couldTerminate day21a_sequence

day21a_main :: IO ()
day21a_main = print day21a_solve

beforeLoop :: Ord a => [a] -> [a]
beforeLoop = go S.empty
  where go _ [] = []
        go seen (x:xs) | x `S.member` seen = []
                       | otherwise = x : go (S.insert x seen) xs

lastUniq :: Ord a => [a] -> a
lastUniq [] = error "lastUniq : empty list"
lastUniq (x : xs) = go (S.singleton x) x xs
  where go _ cur [] = cur
        go seen cur (y : ys) | y `S.member` seen = go seen cur ys
                             | otherwise = go (S.insert y seen) y ys

-- 21b: what is the value of R0 that will terminate the loop the slowest
--  I assume that there are only finitely many pairs (R2,R3) that show up
--  in the execution. If so, this is easy!

day21b_solve :: Integer
day21b_solve = lastUniq $ map snd $ filter ((<256).fst) $ beforeLoop day21a_sequence

day21b_main :: IO ()
day21b_main = print day21b_solve
