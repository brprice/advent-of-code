{-# LANGUAGE TupleSections #-}
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Data.Ix
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Void

import Util

data V2 = V2 {_x, _y :: Int} deriving (Eq, Show, Ix)
instance Ord V2 where
  compare (V2 x1 y1) (V2 x2 y2) = case compare y1 y2 of
                                    LT -> LT
                                    EQ -> compare x1 x2
                                    GT -> GT

(+.) :: V2 -> V2 -> V2
V2 a b +. V2 c d = V2 (a+c) (b+d)

data Watery = Clay | StandingWater | FlowingWater deriving (Eq, Show)

swapV2 :: V2 -> V2
swapV2 (V2 a b) = V2 b a

-- 17a: Simulate water flow (no water pressure)
-- Count the wet squares at time infinity
-- (ignoring the black hole below the region of interest!)
day17a_parser :: Parser (S.Set V2)
day17a_parser = S.unions <$> many (line <* eol)
  where line' :: String -> String -> Parser (S.Set V2)
        line' a b = (\a' b1 b2 -> S.map (V2 a') $ S.fromList [b1..b2]) <$
                    string a <* string "=" <*> decimal <* string ", "
                 <* string b <* string "=" <*> decimal <* string ".." <*> decimal
        line = line' "x" "y"
           <|> (S.map swapV2 <$> line' "y" "x")

-- looks left and right to find a boundary or missing floor
data Extent = Open {_accessible :: [V2], _holes :: [V2]}
            | Closed {_accessible :: [V2]}
  deriving Eq

extent :: V2 -> M.Map V2 Watery -> Extent
extent p scan = case (left, right) of
                  ((lacc,lhs),(racc,rhs)) -> mkExt (lacc++racc) (lhs++rhs)
  where left = go [] (V2 (-1) 0) p
        right = go [] (V2 1 0) p
        go acc d q
          | M.lookup (q+. V2 0 1) scan `elem` [Nothing, Just FlowingWater]
          = (q:acc, [q +. V2 0 1])
          | M.lookup (q +. d) scan `elem` [Just Clay, Just StandingWater]
          = (q:acc, [])
          | otherwise
          = go (q:acc) d (q +. d)
        mkExt acc [] = Closed acc
        mkExt acc hs = Open acc hs

day17a_simulate :: S.Set V2 -> M.Map V2 Watery
day17a_simulate clay | S.null clay = M.empty
day17a_simulate clay = M.filterWithKey (\k _ -> _y k >= top)
                     $ go spring
                     $ M.fromSet (const Clay) clay
  where spring = V2 500 0
        top = _y $ S.findMin clay
        bottom = _y $ S.findMax clay
        go source scan | _y source > bottom = scan
                       | _y source == bottom = M.insert source FlowingWater scan
                       | otherwise = case M.lookup (source +. V2 0 1) scan of
                           Nothing -> go source (go (source +. V2 0 1) scan)
                           Just FlowingWater -> M.insert source FlowingWater scan
                           Just _ -> case extent source scan of
                                       Closed acc -> M.union (M.fromList $ map (,StandingWater) acc) scan
                                       oldExt@(Open acc holes) ->
                                         let filled = foldr go scan holes
                                             newExt = extent source filled
                                             mkFlow = M.fromList $ map(,FlowingWater) acc
                                         in if oldExt == newExt
                                            then M.union mkFlow filled
                                            else go source filled


day17a_solve :: S.Set V2 -> Either Void Int
day17a_solve = pure . M.size . M.filter (/=Clay) . day17a_simulate

-- Not used, but useful for debugging
day17a_show_map :: M.Map V2 Watery -> String
day17a_show_map scan = let keys = M.keys scan
                           top = minimum $ 0 : map _y keys
                           bot = maximum $ map _y keys
                           left = minimum $ map _x keys
                           right = maximum $ map _x keys
                           cell 500 0 = '+' -- the spring
                           cell x y = case M.lookup (V2 x y) scan of
                                        Nothing -> '.'
                                        Just Clay -> '#'
                                        Just StandingWater -> '~'
                                        Just FlowingWater -> '|'
                       in unlines [[cell x y | x <- [left..right]] | y <- [top..bot]]

day17a_main :: IO ()
day17a_main = generic_main "../data/17a" day17a_parser day17a_solve show


-- 17b: Eventually, water stops being produced, how much will be retained
day17b_solve :: S.Set V2 -> Either Void Int
day17b_solve = pure . M.size . M.filter (==StandingWater) . day17a_simulate

day17b_main :: IO ()
day17b_main = generic_main "../data/17a" day17a_parser day17b_solve show
