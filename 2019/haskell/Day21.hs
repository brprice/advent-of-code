module Main where

import Control.Monad.State (evalState)
import Data.Char (ord)

import Intcode


{- Today was a "jump over the holes" game - you either move forward one square, or jump over three.
   It seems impossible to ensure success (i.e. not falling into a hole) with finite look-ahead, but
   these strategies work for my inputs
-}

main :: IO ()
main = do mach <- readIntcode "../data/day21"
          -- jump if there is land 4 tiles away, and a hole before that
          let inA = unlines ["NOT A T"
                            ,"OR T J"
                            ,"NOT B T"
                            ,"OR T J"
                            ,"NOT C T"
                            ,"OR T J"
                            ,"AND D J"
                            ,"WALK"]
          let outA = evalState (run $ map (fromIntegral.ord) inA) mach
          putStr "part a: "
          print $ last outA

          -- Jump if you can land, it isn't immediate doom, and you must soon
          -- (i.e. an optimised version of part a, plus a bit more look-ahead)
          -- D && (E || H) && (!A || !B || !C)
          let inB = unlines ["OR E J"
                            ,"OR H J"
                            ,"AND D J"
                            ,"NOT T T"
                            ,"AND A T"
                            ,"AND B T"
                            ,"AND C T"
                            ,"NOT T T"
                            ,"AND T J"
                            ,"RUN"]
          let outB = evalState (run $ map (fromIntegral.ord) inB) mach
          putStr "part b: "
          print $ last outB
