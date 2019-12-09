module Main where

import Control.Monad.State

import Intcode

main :: IO ()
main = do mach <- readIntcode "../data/day9"
          putStr "part a: "
          print $ head $ evalState (run [1]) mach

          putStr "part b: "
          print $ head $ evalState (run [2]) mach
