module Main where

import Data.List.Extra (minimumOn)

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = let (h,t) = splitAt n xs
              in h : chunks n t

main :: IO()
main = do dat <- readFile "../data/day8"
          let w = 25
          let h = 6
          let digs = map (read . (:[])) $ init dat :: [Int]
          let layers = chunks (h*w) digs
          let best_layer = minimumOn (length.filter(==0)) layers
          let parta = length (filter (==1) best_layer) * length (filter (==2) best_layer)
          putStr "part a: "
          print parta
