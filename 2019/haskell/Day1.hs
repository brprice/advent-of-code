module Main where

import Conduit
import Data.ByteString.Char8 (readInt)
import Data.Maybe (fromJust)

main :: IO ()
main = do (simple,tyrant) <- runConduitRes $ sourceFile "../data/day1a"
                                          .| linesUnboundedAsciiC
                                          .| mapC (fst . fromJust . readInt)
                                          .| mapC toFuel
                                          .| foldlC (\(as,at) (s,t) -> (as+s,at+t)) (0,0)
          putStr "Total fuel needed (simplistic, part a): "
          print simple
          putStr "Total fuel needed (tyrannical, part b): "
          print tyrant

toFuel :: Int -> (Int,Int)
toFuel mass = let iters = tail $ iterate (\m -> m`div`3-2) mass
              in (head iters, sum $ takeWhile (>0) iters)
