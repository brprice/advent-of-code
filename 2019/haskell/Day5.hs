module Main where

import Control.Monad.State

import Intcode

main :: IO ()
main = do mach <- readIntcode "../data/day5"
          let resulta = evalState (run [1]) mach
          let (_,rest) = span (==0) resulta
          case rest of
            [a] -> putStrLn $ "part a: " ++ show a
            nzs -> putStrLn $ "part a: failure: non-zero output was " ++ show nzs

          let resultb = evalState (run [5]) mach
          case resultb of
            [b] -> putStrLn $ "part b: " ++ show b
            _ -> putStrLn $ "part b: failure: non-singleton output was " ++ show resultb
