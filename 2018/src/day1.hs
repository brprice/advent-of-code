import Data.Void
import qualified Data.Set as S

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Util

-- 1a: sum a list

pSInt :: Parser Integer
pSInt = (single '+' *> decimal)
    <|> (negate <$ single '-' <*> decimal)

day1a_parser :: Parser [Integer]
day1a_parser = many (pSInt <* eol)

day1a_solve :: [Integer] -> Either Void Integer
day1a_solve = Right . sum

day1a_main :: IO ()
day1a_main = generic_main "../data/1a" day1a_parser day1a_solve show

-- 1b: running sums of a cyclic list, find the first repetition
day1b_main :: IO ()
day1b_main = generic_main "../data/1a" day1a_parser day1b_solve show

day1b_solve :: [Integer] -> Either Void Integer -- No error other than non-termination
day1b_solve xs = Right $ firstRepetition $ scanl (+) 0 $ cycle xs
  where firstRepetition :: [Integer] -> Integer
        firstRepetition xs = go S.empty xs
        go seen [] = error "cycle not infinite?"
        go seen (x:xs) | x`S.member`seen = x
                       | otherwise = go (S.insert x seen) xs
