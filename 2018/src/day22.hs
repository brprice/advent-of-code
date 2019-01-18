{-# LANGUAGE RankNTypes , ScopedTypeVariables #-} -- DEBUGGING
{-# LANGUAGE TupleSections #-}
import Data.Ix
import qualified Data.Map.Lazy as M
import qualified Data.Map.Strict as MS
import qualified Data.Set as S
import Data.Void

import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Util

-- 22a: sum up values in rectangle which are generated weirdly
day22a_parser :: Parser (Integer, (Integer, Integer))
day22a_parser = (,) <$ string "depth: " <*> decimal <* eol
            <*> ((,) <$ string "target: " <*> decimal <* string "," <*> decimal <* eol)

-- For part a, maxXY = (tx,ty),
-- but for part b, will not
eroLvl :: Integer -> (Integer, Integer) -> (Integer, Integer) -> M.Map (Integer, Integer) Integer
eroLvl depth (tx,ty) maxXY = lvls
  where lvls = M.fromList $ map (\p -> (p,uncurry f p)) $ range ((0,0),maxXY)
        f 0 0 = 0
        f x y | x==tx && y==ty = 0
        f x 0 = (16807 * x + depth)`mod`20183
        f 0 y = (48271 * y + depth)`mod`20183
        f x y = ((lvls M.! (x-1, y))*(lvls M.! (x, y-1)) + depth) `mod` 20183

day22a_solve :: (Integer, (Integer, Integer)) -> Either Void Integer
day22a_solve (depth, target) = let region = range ((0,0), target)
                                   erLvls = eroLvl depth target target
                                   typ p = (erLvls M.! p) `mod` 3
                               in pure $ sum $ map (\p -> typ p) region

day22a_main :: IO ()
day22a_main = generic_main "../data/22a" day22a_parser day22a_solve show


-- 22b: Find fastest route to target, with weird "equipment" restrictions
-- assume we won't go outside a square of size 2*max tx ty
-- though we may go beyond (tx,ty)

data Terrain = Rocky | Wet | Narrow
data Tool = Neither | Torch | Climb deriving (Eq, Ord)
type V2 = (Integer, Integer)
type WeightedGraph a = M.Map a (S.Set (a, Integer))

nhood :: M.Map V2 a -> V2 -> [V2]
nhood m (x,y) = filter (`MS.member` m) [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

erLvlToTerrain :: Integer -> Terrain
erLvlToTerrain n = [Rocky,Wet,Narrow] !! fromIntegral (n`mod`3)

terTool :: Terrain -> [Tool]
terTool Rocky = [Climb,Torch]
terTool Wet = [Climb,Neither]
terTool Narrow = [Torch,Neither]

data Inf a = Fin a | Inf deriving (Eq, Ord, Show)

(+^) :: Num a => Inf a -> Inf a -> Inf a
Inf +^ _ = Inf
_ +^ Inf = Inf
Fin a +^ Fin b = Fin $ a + b

-- "Dynamically generated graph" astar
-- We will only ask for weights of neighbours
-- I.e. if w x y is called, then y `S.member` nbd x
-- Return a (min-length,opt-path) pair for each target we managed to reach
-- We only have optimality guarantees if the heuristic
-- is admissible (only underestimates distance)
-- Similarly, if h a t is called, then t `S.member` _tgts
-- TODO: could probably track "parent" as-we-go, rather than
-- backtracing the distances.
astar :: (Ord a, Num d, Ord d) => (a -> S.Set a) -> (a -> a -> d) -> a -> a -> (a -> Inf d) -> Maybe d --(d,[a])
astar nbd w source tgt h = findPaths $ go (S.singleton (Fin 0 +^ h source, 0, source)) -- Using a set as a poor-man's priority queue here!
                                          (M.singleton source 0)
  where findPaths dists = M.lookup tgt dists
        go front seen = case S.minView front of
                          Nothing -> seen
                          Just ((_,g,a),front')
                            | a == tgt -> seen
                            | otherwise -> let foo b (se, fr) = let gb = g + w a b
                                                                    better = maybe True (>gb) $ M.lookup b se
                                                                    se' = M.insert b gb se
                                                                    fr' = S.insert (Fin gb +^ h b, gb, b) fr
                                                                in if better
                                                                   then (se', fr')
                                                                   else (se, fr)
                                               (seen', front'') = S.foldr foo (seen, front') $ nbd a
                                           in go front'' seen'

manhatten :: (Integer, Integer) -> (Integer, Integer) -> Integer
manhatten (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

day22b_solve :: (Integer, (Integer, Integer)) -> Either String Integer
day22b_solve (depth, target) = let bounds = 2*max (fst target) (snd target)
                                   terrain = MS.map erLvlToTerrain $ eroLvl depth target (bounds,bounds)
                                   nbd (p,t) = S.fromList $ [(p,t') | t'<- terTool $ terrain M.! p, t'/=t]
                                                         ++ [(p',t) | p'<- nhood terrain p,
                                                                      t`elem`terTool(terrain M.! p')]
                                   -- edge weights
                                   w (_,t) (_',t') | t==t' = 1 -- move
                                                   | otherwise = 7 -- swap tool
                                   -- heuristic
                                   h (p,Torch) = manhatten p target
                                   h (p,_) = 7 + manhatten p target
                                   d = astar nbd w ((0,0),Torch) (target,Torch) (Fin . h)
                               in maybe (Left "No path to target?!") pure d

day22b_main :: IO ()
day22b_main = generic_main "../data/22a" day22a_parser day22b_solve show
