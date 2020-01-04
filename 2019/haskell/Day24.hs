{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Bits((.|.),testBit,bit)
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
gridANbd (GA n) (PA (x,y)) = (get x y, [get x' y' | (x',y')<-nbd])
  where nbd = [(x+i,y) | i<-[-1,1], 0<= x+i, x+i<5]
           ++ [(x,y+i) | i<-[-1,1], 0<= y+i, y+i<5]
        get s t = n`testBit`(t*5+s)

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

main :: IO ()
main = do initial <- readGrid <$> readFile "../data/day24"
          let Just (GA partA) = firstRepeat $ iterate evolveA initial
          putStr "part a: "
          print partA
