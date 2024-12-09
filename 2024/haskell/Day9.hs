{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (intersperse)
import GHC.List (foldl')

getData :: IO String
getData = readFile "../data/day9"

data C = Free | File Int
  deriving (Show)

data Block = B {len :: Int, cts :: C}
  deriving (Show)

parse :: String -> [Block]
parse s =
  let [s'] = lines s
      lens = map (read . (: [])) s'
      cs = intersperse Free $ map File [0 ..]
   in zipWith B lens cs

sumup :: [(Int, Int)] -> Int
sumup = fst . foldl' go (0, 0)
  where
    go (acc, pos) (n, f) = (acc + ((n * (2 * pos + n - 1) * f) `div` 2), pos + n)

part1 :: [Block] -> Int
part1 bs = sumup $ go bs (reverse bs)
  where
    go (x : xs) (B n Free : ys) = go (x : xs) ys
    go (B n (File f) : xs) (y : ys) = case y of
      B m (File g) | f == g -> [(m, f)]
      _ -> (n, f) : go xs (y : ys)
    go (B n Free : xs) (B m (File f) : ys)
      | n <= m = (n, f) : go xs (B (m - n) (File f) : ys)
      | otherwise = (m, f) : go (B (n - m) Free : xs) ys

part2 :: [Block] -> Int
part2 bs = sumup $ go bs (reverse bs)
  where
    go (x : xs) (B n Free : ys) = go (x : xs) ys
    go (B n (File f) : xs) ys = (n, f) : go xs (takeWhile (\case B _ Free -> True; B _ (File g) -> g > f) ys)
    go (B n Free : xs) ys =
      case break (\case B m Free -> False; B m (File _) -> m <= n) ys of
        (_, []) -> (n, 0) : go xs ys
        (ys1, B m (File f) : ys2) ->
          (m, f)
            : go
              (B (n - m) Free : map (\case B m (File g) | g == f -> B m Free; x -> x) xs)
              (ys1 ++ ys2)
    go [] [] = []

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
  putStrLn "Part 2"
  print $ part2 xs
