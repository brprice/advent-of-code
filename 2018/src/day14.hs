--import qualified Data.Map as M
import qualified Data.Sequence as S

import Util

-- 14a : build a list of numbers in a funny way

-- no file parsing today!
day14a_input :: Int
day14a_input = 170641

digits :: Integral a => a -> [Int]
digits = reverse . go
  where go n | n < 10 = [fromIntegral n]
             | otherwise = let (q,r) = quotRem n 10
                           in fromIntegral r : go q

-- starts (3) [7], each step:
--  - add digits of sum of brackets to end (here 1,0)
-- - move each set of brackets 1+it's value to right (cycling around)

recipies :: [Int]
recipies = 3:7:go 0 1 2 (S.fromList [3,7])
  where go p1 p2 l xs | p1>=l = go (p1-l) p2 l xs
                      | p2>=l = go p1 (p2-l) l xs
                      | otherwise
          = let v1 = S.index xs p1
                v2 = S.index xs p2
                new = digits $ v1 + v2
                ln = length new
                xs' = xs <> S.fromList new
            in new ++ go (p1+v1+1) (p2+v2+1)
                         (l+ln) xs'

-- 14a: do $INPUT creations of the recipies
-- (NB: one iteration may create 2 recipies)
-- then output the next 10
day14a_solve :: String
day14a_solve = concatMap show $ take 10 $ drop day14a_input recipies


-- 14b: How many recipies appear in the list before $INPUT?

chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = take n xs : chunk n (tail xs)

day14b_solve :: Int
day14b_solve = let ds = digits day14a_input
               in length $ takeWhile (/=ds) $ chunk (length ds) recipies

main = print day14b_solve
