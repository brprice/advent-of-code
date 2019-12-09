module Main where

import Data.List (permutations)
import Control.Monad.State
import Data.Monoid(Dual(Dual,getDual),Endo(Endo,appEndo))

import Intcode

rotateR :: [a] -> [a]
rotateR xs = last xs : init xs

-- Like zipWith, but assumes the first input list is the shorter of the two
-- and thus the output is the same length as the first input list
-- We require this extra laziness, otherwise part b <<loop>>s, because of the line
-- oss = zipWith phased ps $ rotateR oss
zipWithL :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithL _ [] _ = []
zipWithL f (x:xs) ~(y:ys) = f x y : zipWithL f xs ys

main :: IO ()
main = do mach <- readIntcode "../data/day7"
          let phased p is = evalState (run $ p:is) mach

          let phased1 p i = head $ phased p [i]
          let chained ps = appEndo $ getDual $ foldMap (Dual . Endo . phased1) ps
          putStr "part a: maximum thrusters "
          print $ maximum $ map (flip chained 0) $ permutations [0..4]

          let looped ps i = let oss :: [[Int]]
                                oss = zipWithL phased ps $ zipWith (++) ([i] : repeat []) $ rotateR oss
                            in last $ last oss
          putStr "part b: maximum loop-amplified thrusters "
          print $ maximum $ map (flip looped 0) $ permutations [5..9]
