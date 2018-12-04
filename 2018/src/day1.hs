import qualified Data.Set as S

-- 1a: sum a list
day1a_data :: IO [Integer]
day1a_data = map r <$> lines <$> readFile "../data/1a"
  where r :: String -> Integer
        r ('+':n) = read n
        r ('-':n) = negate (read n)

day1a_main :: IO ()
day1a_main = print =<< day1a_solve <$> day1a_data

day1a_solve :: [Integer] -> Integer
day1a_solve = sum

-- 1b: running sums of a cyclic list, find the first repetition
day1b_data :: IO [Integer]
day1b_data = day1a_data

day1b_main :: IO ()
day1b_main = print =<< day1b_solve <$> day1b_data

day1b_solve :: [Integer] -> Integer
day1b_solve xs = firstRepetition $ scanl (+) 0 $ cycle xs
  where firstRepetition :: [Integer] -> Integer
        firstRepetition xs = go S.empty xs
        go seen [] = error "cycle not infinite?"
        go seen (x:xs) | x`S.member`seen = x
                       | otherwise = go (S.insert x seen) xs
