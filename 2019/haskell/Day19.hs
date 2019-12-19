module Main where

import Control.Monad.State (evalState)

import Intcode

main :: IO ()
main = do mach <- readIntcode "../data/day19"
          let inBeam x y = [1] == evalState (run [x,y]) mach
          let parta = length [() | x<-[0..49], y<-[0..49], inBeam x y]
          putStr "part a: "
          print parta

