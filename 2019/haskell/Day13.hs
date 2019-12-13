{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad.State
import qualified Data.Set as S

import Intcode

-- lenses named as the field, with an added underscore
makeLensesWith (lensRules & lensField .~ mappingNamer (\s -> ['_':s])) ''IC

chunk3 :: [a] -> [(a,a,a)]
chunk3 [] = []
chunk3 (x:y:z:xs) = (x,y,z): chunk3 xs
chunk3 _ = error "chunk3: not three elts left"

addRemBlock :: S.Set (Integer,Integer) -> (Integer,Integer,Integer) -> S.Set (Integer,Integer)
addRemBlock blocks (x,y,0) = S.delete (x,y) blocks
addRemBlock blocks (x,y,2) = S.insert (x,y) blocks
addRemBlock blocks _ = blocks

data ICI3O = IC3In (Integer -> IC) | IC3Out Integer Integer Integer IC | IC3Halt IC
runTo3IO :: IC -> ICI3O
runTo3IO ic = case runToIO ic of
  ICIn f -> IC3In f
  ICHalt ic' -> IC3Halt ic'
  ICOut o1 ic1 -> case runToIO ic1 of
    ICOut o2 ic2 -> case runToIO ic2 of
      ICOut o3 ic3 -> IC3Out o1 o2 o3 ic3
      _ -> error "runTo3IO: a block of out was interrupted"
    _ -> error "runTo3IO: a block of out was interrupted"

partb :: Integer -> Integer -> Integer -> IC -> Integer
partb score ballx paddlex ic =
  case runTo3IO ic of
    IC3In f -> partb score ballx paddlex $ f $ case compare ballx paddlex of
      LT -> -1
      EQ -> 0
      GT -> 1
    IC3Out x y ty ic' -> if x == -1 && y == 0
                         then partb ty ballx paddlex ic'
                         else case ty of
                                3 -> partb score ballx x ic' -- paddle
                                4 -> partb score x paddlex ic' -- ball
                                _ -> partb score ballx paddlex ic' -- anything else
    IC3Halt _ -> score

main :: IO ()
main = do mach <- readIntcode "../data/day13"
          let output = evalState (run []) mach
          let blocks = foldl addRemBlock S.empty (chunk3 output)
          putStr "part a: "
          print $ S.size blocks

          -- We will run the game to the end, not just until the last block is broken
          let machb = mach & _mem . at 0 ?~ 2
          putStr "part b: "
          print $ partb 0 0 0 machb -- garbage score, ball and paddle to start with
