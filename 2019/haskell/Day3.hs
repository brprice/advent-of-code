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

toSteps :: [Seg] -> [(Int,Seg)]
toSteps sgs = zip (scanl (\st sg -> st + abs (len sg)) 0 sgs) sgs

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

l1dist :: Point -> Point -> Int
l1dist p q = abs (px p - px q) + abs (py p - py q)

l1norm :: Point -> Int
l1norm = l1dist (Point 0 0)

main :: IO ()
main = do cts <- readFile "../data/day3"
          let [w1,w2] = map (toSteps . toSegs . parse) $ lines cts
          mapM_ print $ take 20 w1
          let intSt (s1,l1) (s2,l2) = (\i -> (i,s1 + s2 + l1dist i (start l1) + l1dist i (start l2))) <$> int l1 l2
          let ints = catMaybes [intSt l1 l2 | l1 <- w1, l2<-w2]
          let (minl1,minInt) = minimumOn fst $ map (\(p,_) -> (l1norm p,p)) ints
          putStrLn $ "Part a: the closest intersection was " ++ show minInt ++ " of l1 norm " ++ show minl1
          let (leastStepsInt, leastSteps) = minimumOn snd ints
          putStrLn $ "Part b: the intersection with least combined path-length was " ++ show leastStepsInt ++ " of combined path-length " ++ show leastSteps
