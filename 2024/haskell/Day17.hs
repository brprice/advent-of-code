{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.State.Strict (State, evalState, get, gets, modify)
import Data.Array (Array, array, bounds, (!))
import Data.Bits (shiftR, xor)
import Data.List (intercalate)
import Data.Monoid (First (First, getFirst))

getData :: IO String
getData = readFile "../data/day17"

data Machine = M
  { ra, rb, rc :: Integer,
    ip :: Int,
    prog :: Array Int Int,
    outputStack :: [Int]
  }

arrayFromList :: [a] -> Array Int a
arrayFromList xs = array (0, length xs - 1) $ zip [0 ..] xs

parse :: String -> Machine
parse s = case map words $ lines s of
  [ ["Register", "A:", a],
    ["Register", "B:", b],
    ["Register", "C:", c],
    [],
    ["Program:", p]
    ] ->
      M
        (read a)
        (read b)
        (read c)
        0
        (arrayFromList $ map (read . (: [])) $ filter (/= ',') p)
        []

getInstr :: State Machine (Maybe (Int, Int))
getInstr = do
  M {ip, prog} <- get
  let (l, u) = bounds prog
  if l <= ip && ip + 1 <= u
    then pure $ Just (prog ! ip, prog ! (ip + 1))
    else pure Nothing

getCombo :: Int -> State Machine Integer
getCombo = \case
  0 -> pure 0
  1 -> pure 1
  2 -> pure 2
  3 -> pure 3
  4 -> gets ra
  5 -> gets rb
  6 -> gets rc

putA :: Integer -> State Machine ()
putA a = modify $ \m -> m {ra = a}

putB :: Integer -> State Machine ()
putB b = modify $ \m -> m {rb = b}

putC :: Integer -> State Machine ()
putC c = modify $ \m -> m {rc = c}

putIP :: Int -> State Machine ()
putIP ip = modify $ \m -> m {ip = ip}

out :: Int -> State Machine ()
out n = modify $ \m@M {outputStack} -> m {outputStack = n : outputStack}

runInstr :: (Int, Int) -> State Machine ()
runInstr = \case
  (0, c) -> putA =<< dv c
  (1, l) -> putB . xor (fromIntegral l) =<< gets rb
  (2, c) -> putB . (`mod` 8) =<< getCombo c
  (3, l) ->
    gets ra >>= \case
      0 -> pure ()
      _ -> putIP $ l - 2 -- minus 2 as we unconditionally add 2 to ip after running an instruction
  (4, _) -> putB =<< xor <$> gets rb <*> gets rc
  (5, c) -> out . fromInteger . (`mod` 8) =<< getCombo c
  (6, c) -> putB =<< dv c
  (7, c) -> putC =<< dv c
  where
    dv c = div <$> gets ra <*> ((2 ^) <$> getCombo c)

runMachine :: State Machine [Int]
runMachine = do
  m <- get
  getInstr >>= \case
    Nothing -> gets $ reverse . outputStack
    Just i -> runInstr i >> modify (\m@M {ip} -> m {ip = ip + 2}) >> runMachine

part1 :: Machine -> String
part1 = intercalate "," . map show . evalState runMachine

{-
-- A naive search is far too slow
part2 :: Machine -> Int
part2 m = fst $ head $ filter ((== elems (prog m)) . snd) $ map (\a -> (a,evalState runMachine m{ra=a})) [0..]
-}

{- Reverse engineering:
Program: 2,4,1,3,7,5,1,5,0,3,4,3,5,5,3,0

Writing out op names, combo args in [_]
bst [4]
bxl 3
cdv [5]
bxl 5
adv [3]
bxc _
out [5]
jnz 0

Expanding combos
bst A
bxl 3
cdv B
bxl 5
adv 3
bxc _
out B
jnz 0

Pseudocode (^ is xor, ** is pow)
do
    B = A%8
    B = B^3
    C = A/(2**B)
    B = B^5
    A = A/(2**3)
    B = B^C
    output B%8
while A/=0

Inlining
do
    B = (A%8)^3
    C = A/(2**B) % 8
    output B^C^5
    A = A/8
while A/=0

Note that we output 1 element for each 3 bits of A, thus A has 16 octal digits,
and each output only depends on the "bits of A to the left"

We can search from the left end of A:
- work out the left octal digit options to output the last element of program
- recurse to find the next digit options (these depend on choices made in first)
- take the smallest A thus found
-}

-- This is hard-coded from reverse-engineering my input
part2 :: Maybe Integer
part2 = getFirst $ findOcts 0 $ reverse prog
  where
    prog = [2, 4, 1, 3, 7, 5, 1, 5, 0, 3, 4, 3, 5, 5, 3, 0]
    findOcts prev [] = First $ Just prev
    findOcts prev (tgt : tgts) =
      let os =
            filter
              (\o -> tgt == o `xor` 6 `xor` (((prev * 8 + o) `shiftR` (fromIntegral o `xor` 3)) `mod` 8))
              [0 .. 7]
       in mconcat $ map (\o -> findOcts (prev * 8 + o) tgts) os

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
  putStrLn "Part 2"
  print part2
