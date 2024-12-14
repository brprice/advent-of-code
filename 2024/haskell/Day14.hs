{-# LANGUAGE LambdaCase #-}

module Main where

getData :: IO String
getData = readFile "../data/day14"

height :: Integer
height = 103

width :: Integer
width = 101

data Robot = R {pos :: (Integer, Integer), vel :: (Integer, Integer)}

parse :: String -> [Robot]
parse = map (parse' . words) . lines
  where
    parse' ['p' : '=' : p, 'v' : '=' : v] = case (break (== ',') p, break (== ',') v) of
      ((px, ',' : py), (vx, ',' : vy)) -> R (read px, read py) (read vx, read vy)

step :: Integer -> Robot -> Robot
step n r@R {pos = (px, py), vel = (vx, vy)} =
  r
    { pos =
        ( (px + n * vx) `mod` width,
          (py + n * vy) `mod` height
        )
    }

quadrants :: [Robot] -> ([Robot], [Robot], [Robot], [Robot])
quadrants = \case
  [] -> ([], [], [], [])
  (r@R {pos = (x, y)} : rs) ->
    let (ul, ur, ll, lr) = quadrants rs
     in case (compare x $ (width - 1) `div` 2, compare y $ (height - 1) `div` 2) of
          (LT, LT) -> (r : ul, ur, ll, lr)
          (LT, GT) -> (ul, ur, r : ll, lr)
          (GT, LT) -> (ul, r : ur, ll, lr)
          (GT, GT) -> (ul, ur, ll, r : lr)
          _ -> (ul, ur, ll, lr)

safetyFactor :: ([Robot], [Robot], [Robot], [Robot]) -> Int
safetyFactor (ul, ur, ll, lr) = length ul * length ur * length ll * length lr

part1 :: [Robot] -> Int
part1 = safetyFactor . quadrants . map (step 100)

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
