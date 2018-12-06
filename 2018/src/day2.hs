import Data.List
import Data.Void
import Control.Monad

import Text.Megaparsec
import Text.Megaparsec.Char

import Util

-- 2a: n = # lines with some char repeated exactly twice
--     m = # ... thrice
--     calculate n*m

day2a_parser :: Parser [String]
day2a_parser = many (many letterChar <* eol)

day2a_main :: IO ()
day2a_main = generic_main "../data/2a" day2a_parser day2a_solve show

day2a_solve :: [String] -> Either Void Int
day2a_solve xs = let twice = filter (rep 2) xs
                     thrice = filter (rep 3) xs
                 in pure $ length twice * length thrice
  where rep n = not . null . filter ((==n).length) . group . sort


-- 2b: find two lines of input that differ in exactly one place
-- return common letters

day2b_main :: IO ()
day2b_main = generic_main "../data/2a" day2a_parser day2b_solve show

-- stupid quadratic time algorithm
day2b_solve :: [String] -> Either String String
day2b_solve xs = let diff1 = do (x:ys) <- tails xs
                                y <- ys
                                guard $ 1 == length (filter not $ zipWith (==) x y)
                                pure $ (x,y)
                 in case diff1 of
                      [(a,b)] -> pure $ do (x,y) <- zip a b
                                           guard $ x == y
                                           pure x
                      [] -> Left "No pair differing in exactly one place"
                      xs -> Left $ "Multiple pairs differing in exactly one place: " ++ show xs
