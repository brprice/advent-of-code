import qualified Data.Map as M
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Util

-- 3a: given a bunch of rectangles, how many square inches are covered by at least two of them
data Claim = Claim {ident :: Int, lt :: (Int,Int), rb :: (Int,Int) }
  deriving Show
left, right, top, bot :: Claim -> Int
left = fst . lt
right = fst . rb
top = snd . lt
bot = snd . rb

instance Eq Claim where
  c == d = ident c == ident d

-- format '#1 @ 2,3: 4x5' means id 1, top left 2,3, width 4, height 5
-- we measure everything from top left corner
day3a_parser :: Parser [Claim]
day3a_parser = many (p <* eol)
  where p :: Parser Claim
        p = foo <$ single '#' <*> decimal <* string " @ "
                <*> decimal <* single ',' <*> decimal <* string ": "
                <*> decimal <* single 'x' <*> decimal
        foo id l t w h = Claim {ident = id, lt=(l,t), rb=(l+w-1,t+h-1)}

day3a_main :: IO ()
day3a_main = generic_main "../data/3a" day3a_parser day3a_solve show

-- build a map of square inch:#claims on it
day3a_solve :: [Claim] -> Either Void Int
day3a_solve cs = Right $ M.size $ M.filter (>1) $ M.fromListWith (+) $ concatMap sqrs cs
sqrs (Claim _ (l,t) (r,b)) = [((x,y),1) | x<-[l..r] , y<-[t..b]]

-- 3b: find the exactly one rectangle that doesn't overlap any other

day3b_main :: IO ()
day3b_main = generic_main "../data/3a" day3a_parser day3b_solve show

day3b_solve :: [Claim] -> Either String Claim
day3b_solve cs = case filter (\c -> all (notClash c) cs) cs of
                   [c] -> Right c
                   cs -> Left $ "not exactly one non-clashing?: " ++ show cs
  where notClash c d = c == d || (bot c < top d || bot d < top c) || (right c < left d || right d < left c)
