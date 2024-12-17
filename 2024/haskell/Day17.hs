{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.State.Strict (State, evalState, get, gets, modify)
import Data.Array (Array, array, bounds, (!))
import Data.Bits (xor)
import Data.List (intercalate)

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

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
