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

part1 :: [Block] -> Int
part1 bs = fst $ foldl' sumup (0, 0) $ go bs (reverse bs)
  where
    sumup (acc, pos) (n, f) = (acc + ((n * (2 * pos + n - 1) * f) `div` 2), pos + n)
    go (x : xs) (B n Free : ys) = go (x : xs) ys
    go (B n (File f) : xs) (y : ys) = case y of
      B m (File g) | f == g -> [(m, f)]
      _ -> (n, f) : go xs (y : ys)
    go (B n Free : xs) (B m (File f) : ys)
      | n <= m = (n, f) : go xs (B (m - n) (File f) : ys)
      | otherwise = (m, f) : go (B (n - m) Free : xs) ys

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
