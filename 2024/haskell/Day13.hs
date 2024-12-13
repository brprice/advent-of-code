{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Maybe (mapMaybe)

getData :: IO String
getData = readFile "../data/day13"

-- P (a,b) (c,d) (e,f) represents
--   a*n + c*m = e
--   b*n + d*m = f
data Puzzle
  = P
      (Integer, Integer)
      (Integer, Integer)
      (Integer, Integer)

parse :: String -> [Puzzle]
parse = map parse' . chunk . map words . lines
  where
    chunk = \case
      (a : b : p : [] : ls) -> (a, b, p) : chunk ls
      [a, b, p] -> [(a, b, p)]
      [] -> []
    parse'
      ( ["Button", "A:", 'X' : '+' : adx', 'Y' : '+' : ady],
        ["Button", "B:", 'X' : '+' : bdx', 'Y' : '+' : bdy],
        ["Prize:", 'X' : '=' : px', 'Y' : '=' : py]
        ) =
        let adx = init adx'
            bdx = init bdx'
            px = init px'
         in P (read adx, read ady) (read bdx, read bdy) (read px, read py)

solvePuzzle :: Puzzle -> Maybe (Integer, Integer)
solvePuzzle (P (a, b) (c, d) (e, f)) =
  let nNumerator = d * e - c * f
      mNumerator = a * f - b * e
      denominator = a * d - b * c
      (nq, nr) = quotRem nNumerator denominator
      (mq, mr) = quotRem mNumerator denominator
   in if nr == 0 && mr == 0 then Just (nq, mq) else Nothing

part1 :: [Puzzle] -> Integer
part1 = sum . map (\(a, b) -> 3 * a + b) . mapMaybe solvePuzzle

part2 :: [Puzzle] -> Integer
part2 = part1 . map (\(P a b (x, y)) -> P a b (x + 10000000000000, y + 10000000000000))

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
  putStrLn "Part 2"
  print $ part2 xs
