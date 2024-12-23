module Main where

import Data.Bifunctor (second)
import Data.List (sort, tails)
import Data.Map.Strict qualified as M
import Data.Set qualified as S

getData :: IO String
getData = readFile "../data/day23"

-- undirected edge, with src < tgt
data Edge = E String String

mkEdge :: String -> String -> Edge
mkEdge a b = if a <= b then E a b else E b a

parse' :: String -> [Edge]
parse' = map (uncurry mkEdge . second tail . break (== '-')) . lines

newtype Graph = G (M.Map String (S.Set String))

toGraph :: [Edge] -> Graph
toGraph =
  G
    . M.unionsWith (<>)
    . map (\(E a b) -> M.fromList [(a, S.singleton b), (b, S.singleton a)])

parse :: String -> Graph
parse = toGraph . parse'

pickTwo :: [a] -> [(a, a)]
pickTwo = concatMap (\(x : xs) -> map (x,) xs) . init . init . tails

part1 :: Graph -> Int
part1 (G g) = S.size $ S.fromList $ concatMap k3Of ts
  where
    vs = M.keys g
    ts = filter ((== 't') . head) vs
    k3Of v =
      let ns = g M.! v
       in map (\(a, b) -> sort [a, b, v]) $
            filter (\(x, y) -> S.member y (g M.! x) || S.member x (g M.! y)) $
              pickTwo $
                S.toList ns

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
