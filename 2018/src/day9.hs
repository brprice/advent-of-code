import Data.Bifunctor
import Data.Function
import Data.List
import qualified Data.List.PointedList.Circular as PL
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Util

-- 9a: play a deterministic marble game, and find the winner's score
day9a_parser :: Parser (Int,Int) -- #players, #marbles
day9a_parser = (,) <$> decimal <* string " players; last marble is worth " <*> decimal <* string " points" <* eol

-- gives trace of who scores what points
play :: Int -> Int -> [(Int,Int)]
play ps ms = step (PL.singleton 0) 1 1
  where next p | p==ps = 1
               | otherwise = p+1
        step marbles p m | m>ms = {-# SCC "[]" #-} []
                         | m`rem`23==0 = {-# SCC "23" #-}
                           let mbls' = PL.moveN (-7) marbles
                               m' = PL._focus mbls'
                               Just mbls = PL.deleteRight mbls'
                           in (p,m+m') : step mbls (next p) (m+1)
                         | otherwise = {-# SCC "otherwise" #-}
                             step ({-# SCC "++" #-} PL.insertRight m (PL.next marbles)) (next p) (m+1)

winningScore :: [(Int,Int)] -> (Int,Int)
winningScore = maximumBy (compare`on`snd) . map (bimap head sum . unzip) . groupBy ((==)`on`fst) . sort

day9a_solve :: (Int,Int) -> Either Void (Int,Int)
day9a_solve = Right . winningScore . uncurry play

day9a_show :: (Int,Int) -> String
day9a_show (p,s) = unwords ["Player",show p,"won, with",show s,"points"]

day9a_main :: IO ()
day9a_main = generic_main "../data/9a" day9a_parser day9a_solve day9a_show

-- 9b: Do the same, with 100 times longer game!
day9b_main :: IO ()
day9b_main = generic_main "../data/9a" day9a_parser (day9a_solve . second (*100)) day9a_show
