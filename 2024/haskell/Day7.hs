{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Functor ((<&>))

getData :: IO String
getData = readFile "../data/day7"

-- target; numbers
-- numbers are stored "backwards": head is rightmost
data Equ = E {tgt :: Integer, inputs :: [Integer]}
  deriving (Show)

parse :: String -> [Equ]
parse s = lines s <&> \(words -> (t : is)) -> E (read $ init t) (reverse $ map read is)

couldBeTrue1 :: Equ -> Bool
couldBeTrue1 = \case
  (E tgt [x]) -> x == tgt
  (E tgt (n : ns)) ->
    let s = couldBeTrue1 $ E (tgt - n) ns
     in case quotRem tgt n of
          (q, 0) -> couldBeTrue1 (E q ns) || s
          _ -> s

part1 :: [Equ] -> Integer
part1 = sum . map tgt . filter couldBeTrue1

numlen :: Integer -> Integer
numlen n = if n < 10 then 1 else 1 + numlen (quot n 10)

couldBeTrue2 :: Equ -> Bool
couldBeTrue2 = \case
  (E tgt [x]) -> x == tgt
  (E tgt (n : ns)) ->
    let s = couldBeTrue2 $ E (tgt - n) ns
        m = let (q, r) = quotRem tgt n in r == 0 && couldBeTrue2 (E q ns)
        c = let (h, t) = quotRem tgt (10^numlen n) in t == n && couldBeTrue2 (E h ns)
     in m || s || c

part2 :: [Equ] -> Integer
part2 = sum . map tgt . filter couldBeTrue2

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
  putStrLn "Part 2"
  print $ part2 xs
