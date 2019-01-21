import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Data.List
import Data.Void

import Util

data V4 = V4 Int Int Int Int deriving Show

-- 25a: count constellations
day25a_parser :: Parser [V4]
day25a_parser = many (star <* eol)
  where star = V4 <$> sInt <* string "," <*> sInt <* string ","
                  <*> sInt <* string "," <*> sInt
        sInt = decimal <|> negate <$ string "-" <*> decimal

manhatten :: V4 -> V4 -> Int
manhatten (V4 a b c d) (V4 a' b' c' d') = sum $ map abs $ zipWith (-) [a,b,c,d] [a',b',c',d']

-- rubbish brute-force
constellations :: [V4] -> [[V4]]
constellations = go []
  where go cs [] = cs
        go cs (s:ss) = go (addS cs s) ss
        addS [] s = [[s]]
        addS cs s = let (near,far) = partition ((<=3).dist s) cs
                    in concat ([s]:near) : far
        dist s c = minimum $ map (manhatten s) c

day25a_solve :: [V4] -> Either Void Int
day25a_solve = pure . length . constellations

day25a_main :: IO ()
day25a_main = generic_main "../data/25a" day25a_parser day25a_solve show

-- 25b: there is no 25b!
