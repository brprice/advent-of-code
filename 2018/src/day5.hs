import Data.Char
import Data.Function
import Data.List
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

import Util

-- 5a: given a word in the free group on [a..z] (inverse given by capitals), reduce the word
-- how long is the reduced word?

data FGElt a = Elt a | Inv a
data FG a = FG [FGElt a]

day5a_parser :: Parser (FG Char)
day5a_parser = FG <$> many elt <* eol
  where elt = Elt <$> lowerChar
          <|> Inv . toLower <$> upperChar

day5a_main :: IO ()
day5a_main = generic_main "../data/5a" day5a_parser day5a_solve show

-- reduce the word
reduce :: Eq a => FG a -> FG a
reduce (FG xs) = FG $ go' [] xs
  where go l (Elt x) (Inv y) r | x == y = go' l r
        go l (Inv x) (Elt y) r | x == y = go' l r
        go l x y [] = reverse (y:x:l)
        go l x y (r:rs) = go (x:l) y r rs

        go' [] (a:b:xs) = go [] a b xs
        go' (a:xs) (b:ys) = go xs a b ys
        go' xs ys = reverse xs ++ ys

len :: FG a -> Int
len (FG xs) = length xs

day5a_solve :: FG Char -> Either Void Int
day5a_solve = pure . len . reduce

shw :: FG Char -> String
shw (FG elts) = map shw' elts
  where shw' (Elt a) = a
        shw' (Inv a) = toUpper a


-- 5b: Map to the free group on ['a'..'z']\\[c], for some c, by taking c and c^-1 to identity
-- i.e. delete all instances of (Elt c) and (Inv c) for some c
-- what is the length of the shortest reduced such word

day5b_main :: IO ()
day5b_main = generic_main "../data/5a" day5a_parser day5b_solve show

-- We'll also give what the best char was
day5b_solve :: FG Char -> Either Void (Int,Char)
day5b_solve = pure . minimumBy (compare`on`fst) . map (\(c,xs) -> (len (reduce xs) , c)) . allRemovals
  where allRemovals (FG xs) = map (\c -> (c , FG $ deleteAll c xs)) ['a'..'z']
        deleteAll c [] = []
        deleteAll c (Elt x:xs) | x == c = deleteAll c xs
                               | otherwise = Elt x : deleteAll c xs
        deleteAll c (Inv x:xs) | x == c = deleteAll c xs
                               | otherwise = Inv x : deleteAll c xs
