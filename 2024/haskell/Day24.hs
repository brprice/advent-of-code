{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Map.Lazy qualified as M

getData :: IO String
getData = readFile "../data/day24"

type W = String

data Input = I W Bool

data Op = And | Or | XOr

data Gate = G W W Op W

parse :: String -> ([Input], [Gate])
parse s =
  let (inputs, _ : gates) = break (== "") $ lines s
   in (map parseInput inputs, map parseGate gates)
  where
    parseInput s =
      let [wc, v] = words s
       in I (init wc) (read v == 1)
    parseGate s =
      let [w1, op, w2, _, out] = words s
          op' = case op of "AND" -> And; "OR" -> Or; "XOR" -> XOr
       in G w1 w2 op' out

fromBits :: [Bool] -> Integer
fromBits = \case
  [] -> 0
  (d : ds) -> fromBits ds * 2 + if d then 1 else 0

-- Huzzah for laziness!
part1 :: ([Input], [Gate]) -> Integer
part1 (is, gs) =
  let m =
        M.fromList $
          [(i, v) | I i v <- is]
            ++ [(w3, interp (m M.! w1) (m M.! w2) o) | G w1 w2 o w3 <- gs]
      interp w1 w2 = \case
        And -> w1 && w2
        Or -> w1 || w2
        XOr -> w1 /= w2
      zs = filter ((== 'z') . head) $ M.keys m
   in fromBits $ map (m M.!) zs

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
