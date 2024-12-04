{-# LANGUAGE DerivingStrategies #-}

module Main where

import Data.Array.Unboxed (Ix, UArray, array, indices, (!?))
import Data.Functor ((<&>))

getData :: IO String
getData = readFile "../data/day4"

newtype I2 = I2 (Int, Int)
  deriving newtype (Eq, Ord, Ix)
  deriving stock (Show)

lI2 :: (Int -> Int) -> I2 -> I2
lI2 f (I2 (x, y)) = I2 (f x, f y)

llI2 :: (Int -> Int -> Int) -> I2 -> I2 -> I2
llI2 f (I2 (a, b)) (I2 (x, y)) = I2 (f a x, f b y)

instance Num I2 where
  (+) = llI2 (+)
  (*) = llI2 (*)
  negate = lI2 negate
  abs = lI2 abs
  signum = lI2 signum
  fromInteger n = I2 (fromInteger n, fromInteger n)

parse :: String -> UArray I2 Char
parse s =
  let ls = lines s
      h = length ls
      w = length $ head ls
   in array (I2 (0, 0), I2 (w - 1, h - 1)) $
        concat $
          (zip [0 ..] ls) <&> \(y, l) ->
            (zip [0 ..] l) <&> \(x, c) ->
              (I2 (x, y), c)

stencils1 :: [[I2]]
stencils1 =
  let f dxdy = scanl (+) (I2 (0, 0)) $ replicate 3 $ I2 dxdy
      dirs = [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx /= 0 || dy /= 0)]
   in map f dirs

part1 :: UArray I2 Char -> Int
part1 ws =
  let isXmas is = Just "XMAS" == traverse (ws !?) is
      xmass' i = filter isXmas $ map (map (i +)) stencils1
      xmass = concatMap xmass' $ indices ws
   in length xmass

stencils2 :: [([I2], [I2])]
stencils2 =
  let u = map I2 [(-1, -1), (0, 0), (1, 1)]
      d = map I2 [(-1, 1), (0, 0), (1, -1)]
   in [(f u, g d) | f <- [id, reverse], g <- [id, reverse]]

part2 :: UArray I2 Char -> Int
part2 ws =
  let isXmas is = Just "MAS" == traverse (ws !?) is
      xmass' i =
        filter (\(l, r) -> isXmas l && isXmas r) $
          map (\(l, r) -> (map (i +) l, map (i +) r)) stencils2
      xmass = concatMap xmass' $ indices ws
   in length xmass

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
  putStrLn "Part 2"
  print $ part2 xs
