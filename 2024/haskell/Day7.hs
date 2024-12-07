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

couldBeTrue :: Equ -> Bool
couldBeTrue = \case
  (E tgt [x]) -> x == tgt
  (E tgt (n : ns)) ->
    let s = couldBeTrue $ E (tgt - n) ns
     in case quotRem tgt n of
          (q, 0) -> couldBeTrue (E q ns) || s
          _ -> s

part1 :: [Equ] -> Integer
part1 = sum . map tgt . filter couldBeTrue

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
