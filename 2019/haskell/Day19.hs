module Main where

import Control.Monad.State (evalState)
import Data.Maybe (mapMaybe)

import Intcode

main :: IO ()
main = do mach <- readIntcode "../data/day19"
          let inBeam x y = [1] == evalState (run [x,y]) mach
          let beam = [(x,y) | x<-[0..49], y<-[0..49], inBeam x y]
          let parta = length beam
          putStr "part a: "
          print parta

          {- part b: As the beam gets wider (presumably monotonically) moving away from the origin,
            if we have two points s and t at the bottom-left and top-right of a square:
            +---t
            |   |
            s---+
            then the whole square is inside the beam iff both s and t are.
            Thus the algorithm is: scan along one side of the beam (t), and find the first time
            that the opposite corner of our square is also in the beam.
            We start at the first bit of beam that is not the origin, as the beam seems to be
            connected, except for an isolated point (0,0)
          -}
          -- Start at the first point of the main beam
          let t0 = beam !! 1
          let nextT (x,y) = head $ filter (uncurry inBeam) [(x+1,y') | y'<-[y..]]
          let ts = iterate nextT t0
          let mkS (x,y) = (x-99,y+99)
          let squares = mapMaybe (\t -> let s = mkS t
                                        in if fst s >= 0 && uncurry inBeam s
                                           then Just (s,t)
                                           else Nothing)
                                 ts
          let (s,t) = head squares
          let (x,y) = (fst s,snd t)
          let partb = 10000*x+y
          putStr "part b: "
          print partb
