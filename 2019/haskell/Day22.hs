module Main where

import Data.List (stripPrefix)

import Utils (extGCD)

{- We represent a shuffle by the linear map (in Z/pZ) which maps index-before to index-after.
I.e. Cut 2 is represented by Sh 1 -2, as the card at index i moves to index 1*i-2.
NB: In my input (and I would expect everyones), the number of cards is prime.
We can invert this shuffle later, enabling us to build it once (in Z), and use it for
both parts a and b (in Z/pZ and Z/qZ)
-}
data Shuffle = Sh {a :: Integer, b :: Integer} deriving Show

-- Work in Z rather than Z/pZ, so we can reuse the result for both parts.
-- The integers grow large, but not ridiculously so.
instance Semigroup Shuffle where
  -- does the shuffle s first, then t (backwards to Endo semigroup)
  Sh u v <> Sh x y = Sh (u*x) (x*v+y)
instance Monoid Shuffle where
  mempty = Sh 1 0

parse :: String -> Shuffle
parse "deal into new stack" = Sh (-1) (-1)
parse s | Just n <- stripPrefix "cut " s = Sh 1 (negate $ read n)
        | Just n <- stripPrefix "deal with increment " s = Sh (read n) 0
parse _ = error "no parse"

shuffleMod :: Integer -> Shuffle -> Shuffle
shuffleMod p (Sh u v) = Sh (u `mod` p) (v `mod` p)

invShuffle :: Integer -> Shuffle -> Shuffle
-- inverse is x |-> (x-v)/u
invShuffle p (Sh u v) | d == 1 = Sh u' ((u'*negate v)`mod`p)
                      | otherwise = error "is p really prime, and Shuffle's a/=0?"
  where (d,_,u') = extGCD p u -- u' is multiplicative inverse of u (assuming d=1)

runShuffle :: Integer -> Shuffle -> Integer -> Integer
runShuffle p (Sh u v) x = (u*x+v) `mod` p

-- Exponentiate by repeated squaring
expSh :: Integer -> Integer -> Shuffle -> Shuffle
expSh p = go
  where go 0 _ = Sh 1 0
        go 1 sh = sh
        go n sh = let (q,r) = quotRem n 2
                      sh1 = go q sh
                      sh2 = shuffleMod p $ sh1 <> sh1
                  in if r == 0
                     then sh2
                     else shuffleMod p $ sh2 <> sh

main :: IO ()
main = do shuffles <- map parse <$> lines <$> readFile "../data/day22"
          let sh = mconcat shuffles

          let pA = 10007
          putStr "part a: "
          print $ runShuffle pA (shuffleMod pA sh) 2019

          let pB = 119315717514047
          let iterB = 101741582076661
          let shB = expSh pB iterB sh
          putStr "part b: "
          print $ runShuffle pB (invShuffle pB shB) 2020
