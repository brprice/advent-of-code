module Main where

import Control.Monad.State
import qualified Data.Set as S

import Intcode

chunk3 :: [a] -> [(a,a,a)]
chunk3 [] = []
chunk3 (x:y:z:xs) = (x,y,z): chunk3 xs
chunk3 _ = error "chunk3: not three elts left"

addRemBlock :: S.Set (Integer,Integer) -> (Integer,Integer,Integer) -> S.Set (Integer,Integer)
addRemBlock blocks (x,y,0) = S.delete (x,y) blocks
addRemBlock blocks (x,y,2) = S.insert (x,y) blocks
addRemBlock blocks _ = blocks

main :: IO ()
main = do mach <- readIntcode "../data/day13"
          let output = evalState (run []) mach
          let blocks = foldl addRemBlock S.empty (chunk3 output)
          putStr "part a: "
          print $ S.size blocks

