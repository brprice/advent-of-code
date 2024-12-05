module Main where

import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.List (foldl')
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S

getData :: IO String
getData = readFile "../data/day5"

type Page = Int

data Constr = C Page Page
  deriving (Show)

newtype Run = Run [Page]
  deriving (Show)

parse :: String -> ([Constr], [Run])
parse s =
  let ls = lines s
      (cs, "" : rs) = break (== "") ls
      parseConstr c =
        let (l, '|' : r) = break (== '|') c
         in C (read l) (read r)
      parseRun r =
        let (p, rs) = break (== ',') r
         in read p : case rs of
              [] -> []
              ',' : ps -> parseRun ps
   in (map parseConstr cs, map (Run . parseRun) rs)

data RunInfo
  = RI {len :: Int, pages :: S.Set Page, forbiddenLater :: S.Set Page}
  | Invalid

instance Semigroup RunInfo where
  Invalid <> b = Invalid
  a <> Invalid = Invalid
  RI ll pl fll <> RI lr pr flr =
    if S.disjoint fll pr
      then RI (ll + lr) (pl <> pr) (fll <> flr)
      else Invalid

instance Monoid RunInfo where
  mempty = RI 0 mempty mempty

info :: M.Map Page (S.Set Page) -> Run -> RunInfo
info mustBeEarlier (Run ps) = foldMap (\p -> RI 1 (S.singleton p) (M.findWithDefault mempty p mustBeEarlier)) ps

mkEarlier :: [Constr] -> M.Map Page (S.Set Page)
mkEarlier = M.unionsWith S.union . map (\(C l r) -> M.singleton r (S.singleton l))

part1 :: ([Constr], [Run]) -> Int
part1 (cs, rs) =
  let earlier = mkEarlier cs
      f r@(Run ps) = case info earlier r of
        Invalid -> Nothing
        RI l _ _ -> Just $ ps !! (div l 2)
   in sum $ mapMaybe f rs

-- One difficulty with part 2 "order the 'bad' runs correctly (with respect to the ordering constraints)"
-- is that the ordering constraints are _circular_!
-- E.g. in my input, I have 12|13, 13|14, 14|12
-- Thus it is not even obvious what the question wants as output.
-- I assume that restricting attention to the numbers in any particular run gives a non-circular constraint system
linearize :: M.Map Page (S.Set Page) -> [Page]
linearize cs =
  let (bottom, earlier) = first M.keys $ M.partition S.null cs
   in go bottom $ M.toList earlier
  where
    go :: [Page] -> [(Page, S.Set Page)] -> [Page]
    go bottom [] = bottom
    go bottom@(_ : _) earlier =
      let remBot (p, es) = case foldl' (flip S.delete) es bottom of
            es'
              | S.null es' -> Left p
              | otherwise -> Right (p, es')
          (bottom', earlier') = partitionEithers $ map remBot earlier
       in bottom ++ go bottom' earlier'

filterEarlier :: M.Map Page (S.Set Page) -> S.Set Page -> M.Map Page (S.Set Page)
filterEarlier earlier ps =
  M.unionWith
    (<>)
    (M.map (S.intersection ps) (M.restrictKeys earlier ps))
    (M.fromSet (const S.empty) ps)

part2 :: ([Constr], [Run]) -> Int
part2 (cs, rs) =
  let earlier = mkEarlier cs
      f r@(Run ps) = case info earlier r of
        Invalid -> Just ps
        RI l _ _ -> Nothing
      badRuns = mapMaybe f rs
      filterEarlier ps =
        M.unionWith
          (<>)
          (M.map (S.intersection ps) (M.restrictKeys earlier ps))
          (M.fromSet (const S.empty) ps)
      selectMid l = l !! (length l `div` 2)
   in sum $ map (\ps -> selectMid $ linearize $ filterEarlier $ S.fromList ps) badRuns

main :: IO ()
main = do
  xs <- parse <$> getData
  putStrLn "Part 1"
  print $ part1 xs
  putStrLn "Part 2"
  print $ part2 xs
