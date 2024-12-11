{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

getData :: IO String
getData = readFile "../data/day11"

parse :: String -> [Integer]
parse = map read . words

split :: Integer -> Maybe (Integer, Integer)
split n = go 0 1 n 0
  where
    -- l=10^?, h=10*l, l<=b<h (assuming no leading zeros),
    -- concatenation of a,b is original number i.e. a*h+b = n
    go l h a b
      | a < l = Nothing
      | a < h = Just (a, b)
      | otherwise = let (q, r) = quotRem a 10 in go h (10 * h) q (r * h + b)

step :: Integer -> Either Integer (Integer, Integer)
step = \case
  0 -> Left 1
  n
    | Just (l, r) <- split n -> Right (l, r)
    | otherwise -> Left $ 2024 * n

part1 :: [Integer] -> Int
part1 ns = length $ iterate (concatMap (either pure (\(a, b) -> [a, b]) . step)) ns !! 25

type OpenRec a = a -> a

childCount' :: OpenRec (Integer -> [Integer])
childCount' f = \n -> 1 : either f (\(a, b) -> zipWith (+) (f a) (f b)) (step n)

-- childCount :: Integer -> [Integer]
-- childCount = fix childCount'

data MemoTrie a = MT {here :: a, t0, t1 :: (MemoTrie a)}

lookupMT :: MemoTrie a -> Integer -> a
lookupMT t = \case
  0 -> t.here
  n -> case quotRem n 2 of
    (q, 0) -> lookupMT t.t0 q
    (q, 1) -> lookupMT t.t1 q

buildMT :: (Integer -> a) -> MemoTrie a
buildMT f = go 1 0
  where
    go n r = MT (f r) (go (n * 2) r) (go (n * 2) (n + r))

memo :: OpenRec (Integer -> a) -> (Integer -> a)
memo f =
  let t = buildMT (f l)
      l = lookupMT t
   in l

childCount :: Integer -> [Integer]
childCount = memo childCount'

part2 :: [Integer] -> Integer
part2 = sum . map ((!! 75) . childCount)

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
  putStrLn "Part 2"
  print $ part2 xs
