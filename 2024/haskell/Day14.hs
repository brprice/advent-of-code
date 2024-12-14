{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad (void)
import Data.List (sort)
import Data.Map qualified as M

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

concentration :: Integer -> [Robot] -> Integer
concentration nbdRadius =
  let nbd (x, y) = [(x + dx, y + dy) | dx <- [-nbdRadius .. nbdRadius], dy <- [-nbdRadius .. nbdRadius]]
   in \rs ->
        let ps = M.fromListWith (+) $ map ((,1) . pos) rs
         in sum $ map (\p -> sum $ [M.findWithDefault 0 n ps | n <- nbd p]) $ M.keys ps

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs

  let mkLine x rxs
        | x == width = ""
        | otherwise = case rxs of
            [] -> replicate (fromIntegral $ width - x) '.'
            rx : rxs'
              | x == rx -> '#' : mkLine (x + 1) rxs'
              | otherwise -> '.' : mkLine (x + 1) rxs
      draw1 l xs
        | l == height = pure ()
        | otherwise =
            let (map snd -> thisLine, rest) = span ((== l) . fst) xs
             in do
                  putStrLn $ mkLine 0 thisLine
                  draw1 (l + 1) rest
      draw xs = draw1 0 $ sort $ map (\R {pos = (x, y)} -> (y, x)) xs
      go best s ys =
        let c = concentration 2 ys
         in if c > best
              then do
                draw ys
                putStrLn $ "New high point (pictured above): step " ++ show s ++ "\t has concentration " ++ show c ++ "\t(press enter to continue)"
                void getLine
                go c (s + 1) (map (step 1) ys)
              else go best (s + 1) (map (step 1) ys)
  putStrLn "Part 2 is interactive! Press enter to start"
  void getLine
  go 0 0 xs
