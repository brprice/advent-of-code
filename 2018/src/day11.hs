import Data.Array
import Data.Maybe

import Util

-- 11a: Find the "best" 3x3 square from a grid where each cell has a odd scoring

-- No file parsing today!
day11a_input :: Int
day11a_input = 9424

cells :: Array (Int,Int) Int
cells = array ((1,1),(300,300)) [((x,y),cell' x y) | x<-[1..300], y<-[1..300]]

cell' :: Int -> Int -> Int
cell' x y = let r = x+10
            in hundreds ((r*y+day11a_input)*r) - 5

cell :: Int -> Int -> Int
cell x y = cells ! (x,y)

hundreds :: Int -> Int
hundreds n = (n`div`100)`rem`10

subs :: Int -> [(Int,(Int,Int))]
subs s = [(sub x y,(x,y)) | x<-[1..300-s+1], y<-[1..300-s+1]]
  where sub x y = sum [cell x' y' | x'<-[x..x+s-1], y'<-[y..y+s-1]]

day11a_solve :: (Int, Int)
day11a_solve = snd $ maximum $ subs 3

day11a_main :: IO ()
day11a_main = print $ day11a_solve

-- 11b: best of any size
-- the naive way is too slow.
-- let's build a Summed-Area-Table

(!?) :: Ix i => Array i e -> i -> Maybe e
a !? i | inRange (bounds a) i = Just $ a ! i
       | otherwise = Nothing

sat :: Num a => Array (Int,Int) a -> Array (Int,Int) a
sat vs = let (tl,br) = bounds vs
             f x y = vs ! (x,y) + sum (catMaybes [tbl !? (x-1,y)
                                                 ,tbl !? (x,y-1)
                                                 ,negate <$> tbl !? (x-1,y-1)])
             tbl = array (tl,br) [((x,y),f x y) | (x,y)<-indices vs]
         in tbl


-- assumes a SAT table as input
sumSquare sat s (x,y) = sum . catMaybes $
                        [sat !? (x+s-1,y+s-1)
                        ,negate <$> sat !? (x+s-1,y-1)
                        ,negate <$> sat !? (x-1,y+s-1)
                        ,sat !? (x-1,y-1)]

day11b_solve :: [Int] -> (Int, Int, Int)
day11b_solve ss = let sums = sat cells
                  in snd $ maximum [(sumSquare sums s (x,y),(x,y,s))
                                   | s<-ss
                                   , x<-[1..300-s+1]
                                   , y<-[1..300-s+1]]

day11b_main :: IO ()
day11b_main = print $ day11b_solve [1..300]

main = day11b_main
