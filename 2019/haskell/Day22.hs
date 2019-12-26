module Main where

import Data.List (elemIndex, sort, stripPrefix)
import Data.Monoid (Dual(Dual, getDual), Endo(Endo, appEndo))

data Shuffle = Stack | Cut Int | Incr Int deriving Show

parse :: String -> Shuffle
parse "deal into new stack" = Stack
parse s | Just n <- stripPrefix "cut " s = Cut $ read n
        | Just n <- stripPrefix "deal with increment " s = Incr $ read n
parse _ = error "no parse"

stack :: [Integer] -> [Integer]
stack = reverse

cut :: Int -> [Integer] -> [Integer]
cut n cards | n >= 0 = let (h,t) = splitAt n cards
                       in t ++ h
            | otherwise = let (h,t) = splitAt (length cards + n) cards
                          in t ++ h

-- inefficient, but good enough
deal :: Int -> [Integer] -> [Integer]
deal n cards = map snd $ sort $ go 0 cards
  where l = length cards
        go _ [] = []
        go m (c:cs) = (m,c) : go ((m + n) `rem` l) cs

shuffle :: Shuffle -> [Integer] -> [Integer]
shuffle Stack = stack
shuffle (Cut n) = cut n
shuffle (Incr n) = deal n

main :: IO ()
main = do shuffles <- map parse <$> lines <$> readFile "../data/day22"
          let initial = [0..10007-1]
          let final = (getDual $ foldMap (Dual . Endo . shuffle) shuffles) `appEndo ` initial
          let partA = elemIndex 2019 final
          mapM_ print $ partA
