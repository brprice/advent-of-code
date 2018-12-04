import Data.List
import Control.Monad

-- 2a: n = # lines with some char repeated exactly twice
--     m = # ... thrice
--     calculate n*m
day2a_data :: IO [String]
day2a_data = lines <$> readFile "../data/2a"

day2a_main :: IO ()
day2a_main = print =<< day2a_solve <$> day2a_data

day2a_solve :: [String] -> Int
day2a_solve xs = let twice = filter (rep 2) xs
                     thrice = filter (rep 3) xs
                 in length twice * length thrice
  where rep n = not . null . filter ((==n).length) . group . sort

-- 2b: find two lines of input that differ in exactly one place
-- return common letters
day2b_data :: IO [String]
day2b_data = day2a_data

day2b_main :: IO ()
day2b_main = print =<< day2b_solve <$> day2b_data

-- stupid quadratic time algorithm
--day2b_solve :: [String] -> String
day2b_solve xs = let [(a,b)] = do (x:ys) <- tails xs
                                  y <- ys
                                  guard $ 1 == length (filter not $ zipWith (==) x y)
                                  pure $ (x,y)
                 in do (x,y) <- zip a b
                       guard $ x == y
                       pure x
