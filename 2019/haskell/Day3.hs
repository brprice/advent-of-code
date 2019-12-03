module Main where

import Data.List.Extra (minimumOn)
import Data.Maybe (catMaybes)

data Dir = V | H
  deriving (Show, Eq)

data Point = Point {px,py :: Int}
  deriving Show

data Seg = Seg {start :: Point, dir :: Dir, len :: Int}
  deriving Show

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x = go []
  where go acc (y:ys) | x == y = reverse acc : go [] ys
                      | otherwise = go (y:acc) ys
        go acc [] = [reverse acc]

parse :: String -> [(Dir,Int)]
parse = map (\(d:l) -> go d (read l)) . splitOn ','
  where go 'U' l = (V,l)
        go 'D' l = (V,-l)
        go 'L' l = (H,-l)
        go 'R' l = (H,l)
        go d _ = error $ "unexpected direction: " ++ [d]

add :: Point -> Dir -> Int -> Point
add (Point x y) V l = Point x (y+l)
add (Point x y) H l = Point (x+l) y

toSegs :: [(Dir,Int)] -> [Seg]
toSegs = go $ Point 0 0
  where go _ [] = []
        go p ((d,l):sgs) = Seg p d l : go (add p d l) sgs

inside :: Int -> (Int,Int) -> Bool
inside x (a,b) = (a <= x && x <= b) || (b <= x && x <= a)

int :: Seg -> Seg -> Maybe Point
int (Seg s1 H l1) (Seg s2 V l2) = let x = px s2
                                      y = py s1
                                  in if x `inside` (px s1, px s1 + l1)
                                     && y `inside` (py s2, py s2 + l2)
                                     then Just $ Point x y
                                     else Nothing
int (Seg s1 V l1) (Seg s2 H l2) = let x = px s1
                                      y = py s2
                                  in if x `inside` (px s2, px s2 + l2)
                                     && y `inside` (py s1, py s1 + l1)
                                     then Just $ Point x y
                                     else Nothing
int _ _ = Nothing -- Assume wire segments never overlap if they are parallel

l1norm :: Point -> Int
l1norm p = abs (px p) + abs (py p)

main :: IO ()
main = do cts <- readFile "../data/day3"
          let [w1,w2] = map (toSegs . parse) $ lines cts
          let ints = catMaybes [int l1 l2 | l1 <- w1, l2<-w2]
          let (minl1,minInt) = minimumOn fst $ map (\p -> (l1norm p,p)) ints
          putStrLn $ "Part a: the closest intersection was " ++ show minInt ++ " of l1 norm " ++ show minl1
