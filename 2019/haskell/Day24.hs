{-# LANGUAGE DeriveFunctor,FlexibleContexts,TupleSections #-}
module Main where

import Control.Comonad
import Data.Bits((.|.),testBit,bit,popCount)
import Data.Coerce
import Data.List(foldl')
import qualified Data.Map as M
import qualified Data.IntSet as IS

import Utils (parseCharArray)

rule :: Bool -> [Bool] -> Bool
rule True ns = length (filter id ns) == 1
rule False ns = length (filter id ns) `elem` [1,2]

-- represent a 5x5 grid of bools as a list of bits, reading in
-- "biodiversity order", i.e. left-to-right, top-to-bottom, lowest bit first
newtype GridA = GA Int
newtype PosA = PA (Int,Int) -- invariant: 0 to 4

-- given a location, get elt at that location, and its 2 to 4 neighbours
gridANbd :: GridA -> PosA -> (Bool,[Bool])
gridANbd g p@(PA (x,y)) = (get g p, [get g (PA (x',y')) | (x',y')<-nbd])
  where nbd = [(x+i,y) | i<-[-1,1], 0<= x+i, x+i<5]
           ++ [(x,y+i) | i<-[-1,1], 0<= y+i, y+i<5]

get :: GridA -> PosA -> Bool
get (GA n) (PA (s,t)) = n`testBit`(t*5+s)

buildGrid :: (PosA -> Bool) -> GridA
buildGrid f = GA $ foldl' (\n i -> if f (PA $ swap $ yx i)
                             then n .|. bit i
                             else n)
                         0 [0..24]
  where yx i = i`quotRem`5
        swap (x,y) = (y,x)

evolveA :: GridA -> GridA
evolveA g = buildGrid $ uncurry rule . gridANbd g

readGrid :: String -> GridA
readGrid s = buildGrid $ \(PA (x,y)) -> parseCharArray s M.! (toInteger x,toInteger y) == '#'

firstRepeat :: Coercible a Int => [a] -> Maybe a
firstRepeat = go IS.empty
  where go _ [] = Nothing
        go seen (x:xs) | coerce x `IS.member` seen = Just x
                       | otherwise = go (IS.insert (coerce x) seen) xs

-- "Cellular automata are comonadic"
-- where our cells are 5x5 grids missing their centre
-- and "left"/"right" is zoom out to a containing grid/zoom in to the middle grid
data Stream a = a :| Stream a deriving Functor

iterateS :: (a -> a) -> a -> Stream a
iterateS f x = x :| iterateS f (f x)

tailS :: Stream a -> Stream a
tailS (_:|xs) = xs

(!) :: Stream a -> Int -> a
(a:|_) ! 0 = a
(_:|as) ! n = as ! (n-1)

data U a = U (Stream a) -- bigger and bigger grids
             a
             (Stream a) -- smaller and smaller grids (middle tile)
  deriving Functor

mid :: U a -> U a
mid (U bs g (m:|ms)) = U (g :| bs) m ms

big :: U a -> U a
big (U (b:|bs) g ms) = U bs b (g:|ms)

instance Comonad U where
  extract (U _ a _) = a
  duplicate u = U (tailS $ iterateS big u) u (tailS $ iterateS mid u)

evolve :: (U a -> a) -> U a -> Stream (U a)
evolve f u = iterateS (f <<=) u

gridBNbd :: U GridA -> PosA -> (Bool,[Bool])
gridBNbd gs p@(PA (x,y)) = (cur `get` p , concatMap f nbs)
  where cur = extract gs
        outer = extract $ big gs
        inner = extract $ mid gs
        nbs = concat [[(x+i,y),(x,y+i)] | i<-[-1,1]]
        f q@(u,v) -- out to left: get (1,2) of previous level
                  | u < 0 = [outer `get` PA (1,2)]
                  -- out to right: get (3,2) of previous level
                  | u >= 5 = [outer `get` PA (3,2)]
                  -- out to top: get (2,1) of previous level
                  | v < 0 = [outer `get` PA (2,1)]
                  -- out to bottom: get (2,3) of previous level
                  | v >= 5 = [outer `get` PA (2,3)]
                  -- into middle: get one entire row/column of next level
                  | u == 2 && v == 2 = case (x,y) of
                      (1,_) -> map (get inner . PA . (0,)) [0..4] --get left edge
                      (3,_) -> map (get inner . PA . (4,)) [0..4] --get right edge
                      (_,1) -> map (get inner . PA . (,0)) [0..4] --get top edge
                      (_,3) -> map (get inner . PA . (,4)) [0..4] --get bottom edge
                      _ -> error "to get to 2,2, must have started at (2+-1,2+-1)"
                  -- finally, easy case: same level
                  | otherwise = [cur `get` (PA q)]

evolveB :: U GridA -> GridA
evolveB u = buildGridB $ uncurry rule . gridBNbd u

-- this ensures (2,2) is always false
buildGridB :: (PosA -> Bool) -> GridA
buildGridB f = buildGrid (\p@(PA (x,y)) -> if x==2&&y==2 then False else f p)

-- truncate each stream to a finite length
trunc :: Int -> U a -> ([a],a,[a])
trunc n (U bs a ms) = (takeS n bs,a,takeS n ms)
  where takeS :: Int -> Stream a -> [a]
        takeS 0 _ = []
        takeS m (x:|xs) = x:takeS (m-1) xs

count :: ([GridA],GridA,[GridA]) -> Int
count (bs,g,ms) = sum (map f bs) + f g + sum (map f ms)
  where f :: GridA -> Int
        f (GA n) = popCount n

main :: IO ()
main = do initial <- readGrid <$> readFile "../data/day24"
          let Just (GA partA) = firstRepeat $ iterate evolveA initial
          putStr "part a: "
          print partA

          -- for part b, we ensure the middle square is ignored and always empty
          let emptys = iterateS id $ GA 0
          let initialB = U emptys initial emptys
          let partB = evolve evolveB initialB ! 200
          putStr "part b: "
          print $ count $ trunc 100 partB -- can only jump 1 level every 2 ticks
