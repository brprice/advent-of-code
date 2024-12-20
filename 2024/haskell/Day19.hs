{-# LANGUAGE LambdaCase #-}

module Main where

getData :: IO String
getData = readFile "../data/day19"

parse :: String -> ([String], [String])
parse s = case lines s of
  (ts : "" : ps) -> (map init (words $ ts ++","), ps)

part1 :: ([String], [String]) -> Int
part1 (ts, ps) = length $ filter (expressible $ map (\s -> (length s, s)) ts) ps
  where
    expressible ts = \case
      "" -> True
      p -> any (\(l, t) -> let (x, y) = splitAt l p in x == t && expressible ts y) ts

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
