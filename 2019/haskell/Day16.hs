module Main where

import Data.List (foldl')

-- the units digit, where trunc (-17) = 7
trunc :: Integer -> Integer
trunc = (`mod` 10).abs

fft :: [Integer] -> [Integer] -> [Integer]
fft pat dat = zipWith (const go) dat $ map (tail . stretch pat) [1..] -- dat here is unused, except for its length
  where stretch xs n = cycle $ concatMap (replicate n) xs
        go p = trunc $ sum $ zipWith (*) dat p

digitsToInteger :: [Integer] -> Integer
digitsToInteger = foldl' (\n d -> n*10 + d) 0

main :: IO ()
main = do dat' <- readFile "../data/day16"
          let dat = map (read . (:[])) $ init dat' -- remove trailing newline

          putStr "part a: "
          print $ digitsToInteger $ take 8 $ iterate (fft [0,1,0,-1]) dat !! 100
