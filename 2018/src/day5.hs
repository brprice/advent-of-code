import Data.Char
import Data.Function
import Data.List

-- 5a: given a word in the free group on [a..z] (inverse given by capitals), reduce the word
-- how long is the reduced word?

data FGElt a = Elt a | Inv a
data FG a = FG [FGElt a]

day5a_data :: IO (FG Char)
day5a_data = parse <$> readFile "../data/5a"

parse :: String -> FG Char
parse = FG . map p . filter isAlpha
  where p c | isUpper c = Inv (toLower c)
            | otherwise = Elt c

day5a_main :: IO ()
day5a_main = print =<< day5a_solve <$> day5a_data

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

day5a_solve :: FG Char -> Int
day5a_solve = len . reduce

shw :: FG Char -> String
shw (FG elts) = map shw' elts
  where shw' (Elt a) = a
        shw' (Inv a) = toUpper a

-- 5b: Map to the free group on ['a'..'z']\\[c], for some c, by taking c and c^-1 to identity
-- i.e. delete all instances of (Elt c) and (Inv c) for some c
-- what is the length of the shortest reduced such word
day5b_data :: IO (FG Char)
day5b_data = day5a_data

day5b_main :: IO ()
day5b_main = print =<< day5b_solve <$> day5b_data

-- We'll also give what the best char was
day5b_solve :: FG Char -> (Int,Char)
day5b_solve = minimumBy (compare`on`fst) . map (\(c,xs) -> (len (reduce xs) , c)) . allRemovals
  where allRemovals (FG xs) = map (\c -> (c , FG $ deleteAll c xs)) ['a'..'z']
        deleteAll c [] = []
        deleteAll c (Elt x:xs) | x == c = deleteAll c xs
                               | otherwise = Elt x : deleteAll c xs
        deleteAll c (Inv x:xs) | x == c = deleteAll c xs
                               | otherwise = Inv x : deleteAll c xs
