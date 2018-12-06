import Data.Function
import Data.List
import Data.Maybe
import Data.Ord

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Util

type Point = (Int,Int)

-- 6a: Find biggest non-infinite Voronoi cell, based on Manhatten distance
day6a_parser :: Parser [Point]
day6a_parser = many (line <* eol)
  where line = (,) <$> decimal <* string ", " <*> decimal

dist :: (Int,Int) -> (Int,Int) -> Int
dist (x,y) (a,b) = abs (x-a) + abs (y-b)

-- Let's find the center of the cell, and its area
-- idea: do cells in the bounding box, everything that reaches edge is infinite
day6a_solve :: [Point] -> Either String (Point,Int)
day6a_solve [] = Left "No given centers?"
day6a_solve ps = pure $ (\l -> (head l,length l)) $ maximumBy (comparing length) $ group $ sort onlyFiniteCells
  where bb@((l,t),(r,b)) = ((minimum $ map fst ps, minimum $ map snd ps)
                           ,(maximum $ map fst ps, maximum $ map snd ps))
        cells = [[cell (x,y) ps | x<-[l..r]] | y<-[t..b]]
        edge = catMaybes $ nub $ sort $ head cells ++ last cells ++ concatMap (\xs -> [head xs,last xs]) cells
        cell (x,y) ps = case groupBy ((==)`on`fst) $ sort $ map (\p -> (dist (x,y) p,p)) ps of
                          ([(_,p)]:ps) -> Just p
                          _ -> Nothing
        onlyFiniteCells = catMaybes $ map (maybe Nothing (\c -> if c`elem`edge then Nothing else Just c)) $ concat cells

day6a_main :: IO ()
day6a_main = generic_main "../data/6a" day6a_parser day6a_solve show


-- 6b: find #cells x such sum d(x,p) for p in given points is at most 10000

-- stupid brute force
day6b_solve :: [Point] -> Either String Int
day6b_solve [] = Left "No given centers?"
day6b_solve ps = pure $ length $ filter ((<m).totDist) [(x,y) | x<-[sl..sr], y<-[st..sb]]
  where bb@((l,t),(r,b)) = ((minimum $ map fst ps, minimum $ map snd ps)
                           ,(maximum $ map fst ps, maximum $ map snd ps))
        n = length ps
        m = 10000
        -- outside searchBox guaranteed to be bad
        searchBox@((sl,st),(sr,sb)) = ((l - (abs (m - n*l - sum (map fst ps)))`div`n -1
                                        , t - (abs (m - n*t - sum (map snd ps)))`div`n -1)
                                      ,(r + (abs (m - n*r - sum (map fst ps)))`div`n +1
                                        , b + (abs (m - n*b - sum (map snd ps)))`div`n))
        totDist p = sum $ map (dist p) ps


day6b_main :: IO ()
day6b_main = generic_main "../data/6a" day6a_parser day6b_solve show
