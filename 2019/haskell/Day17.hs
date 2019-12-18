module Main where

import Control.Monad.State (evalState)
import Data.Char (chr)
import qualified Data.Set as S

import Intcode

main :: IO ()
main = do robot <-readIntcode "../data/day17"
          let scaffold = lines $ map (chr.fromInteger) $ evalState (run []) robot
          let scafXY = concat $ zipWith (\y l -> zipWith (\x c -> ((x,y),c)) [0..] l) [0..] scaffold
          let scafL = map fst $ filter (flip elem ['#','^','>','<','v'].snd) scafXY
          let scafS = S.fromList scafL
          let intersections = filter (\(x,y) -> all (\(dx,dy) -> (x+dx,y+dy) `S.member` scafS) [(-1,0),(1,0),(0,-1),(0,1)]) scafL
          let parta :: Integer
              parta = sum $ map (uncurry (*)) intersections
          putStr "part a: "
          print parta

