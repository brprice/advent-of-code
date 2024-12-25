{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (find, intercalate, sort)
import Data.Map.Lazy qualified as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Set qualified as S

getData :: IO String
getData = readFile "../data/day24"

type W = String

data Input = I W Bool

data Op = And | Or | XOr
  deriving (Eq)

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
       in G (min w1 w2) (max w1 w2) op' out

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

part2 :: [Gate] -> String
part2 gs = intercalate "," $ S.toAscList badWires
  where
    out (G _ _ _ o) = o
    ins (G i1 i2 _ _) = [i1, i2]
    op (G _ _ op _) = op
    xys0 = filter (\(G (a : _) (b : _) _ _) -> a == 'x' || b == 'y') gs
    xys = filter ((/= ["x00", "y00"]) . ins) xys0
    aOut = S.fromList $ out <$> filter ((== XOr) . op) xys
    bOut = S.fromList $ out <$> filter ((== And) . op) xys
    cInGates = filter (\(G (a : _) (b : _) op _) -> a /= 'x' && b /= 'y' && op /= Or) gs
    acIns =
      let aci1 = S.fromList $ ins =<< filter ((== XOr) . op) cInGates
          aci2 = S.fromList $ ins =<< filter ((== And) . op) cInGates
       in if aci1 == aci2
            then aci1
            else error "mismatched aIns"
    zOut = S.fromList $ out <$> filter ((== XOr) . op) cInGates
    dOut = S.fromList $ out <$> filter ((== And) . op) cInGates
    orGates = filter ((== Or) . op) gs
    bdIns = S.fromList $ ins =<< orGates
    zmax = maximum $ filter ((== 'z') . head) $ out <$> gs
    cOuts = S.fromList $ filter (/= zmax) $ out <$> orGates
    badWires =
      S.unions
        [ aOut S.\\ acIns,
          bOut S.\\ bdIns,
          S.filter ((/= 'z') . head) zOut,
          dOut S.\\ bdIns,
          cOuts S.\\ acIns
        ]

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
  putStrLn "Part 2"
  print $ part2 $ snd xs
