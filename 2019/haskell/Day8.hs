module Main where

import Data.List (foldl')
import Data.List.Extra (minimumOn)

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = let (h,t) = splitAt n xs
              in h : chunks n t

data Colour = Black
            | White
            | Alpha
  deriving (Eq,Enum)

charColour :: Colour -> Char
charColour Black = ' '
charColour White = '#'
charColour Alpha = '-'

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

          let layerColors = map (map toEnum) layers
          let alpha = replicate (w*h) Alpha
          let dbg lay = unlines $ chunks w $ map charColour lay
          let super img lay = zipWith (\ic lc -> if ic == Alpha then lc else ic) img lay
          let img = foldl' (\i l -> super i l) alpha $ layerColors
          putStrLn "part b:"
          mapM_ (\row -> putStrLn $ map charColour row) $ chunks w img
