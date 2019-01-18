import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Data.List (maximumBy)
import qualified Data.Set as S
import Data.Ord (comparing)
import Data.Void (Void)

import Util

data V3 = V3 { _x, _y, _z :: Int} deriving (Eq, Ord, Show)
data Bot = Bot {_pos :: V3, _range :: Int}

-- 23a: Find number of points in a region
day23a_parser :: Parser [Bot]
day23a_parser = many (bot <* eol)
  where bot = Bot <$ string "pos=<" <*> v3
                  <* string "r=" <*> decimal
        v3 = V3 <$> sInt <* string "," <*> sInt <* string "," <*> sInt <* string ">, "
        sInt = decimal
           <|> negate <$ char '-' <*> decimal

manhatten :: V3 -> V3 -> Int
manhatten (V3 a b c) (V3 e f g) = abs (a-e) + abs (b-f) + abs (c-g)


day23a_solve :: [Bot] -> Either Void Int
day23a_solve bs = let Bot p r = maximumBy (comparing _range) bs
                  in pure $ length $ filter ((<=r) . manhatten p . _pos) bs

day23a_main :: IO ()
day23a_main = generic_main "../data/23a" day23a_parser day23a_solve show


-- 23b: Which points are in the most regions
-- Idea: make an oct-tree. For each cube, count (#regions containing,#regions intersecting)
-- These tell us for an arbitrary point in that region (min #regions it may be in, max #regions it may be in)
-- Then can prune the search tree.
-- However, we can make the oct-tree implicit, and just have regions in a priority queue
-- Actually, we end up doing a kd-tree!

type Cube = (V3,V3) -- min, max in each dimension
type Bounds = (Int, Int)

vertices :: Cube -> [V3]
vertices (V3 x y z, V3 x' y' z') = [V3 a b c | a<-[x,x'], b<-[y,y'], c<-[z,z']]

intersect :: Cube -> Bot -> Bool
intersect (V3 x y z, V3 x' y' z') (Bot b@(V3 bx by bz) r)
  = let clamp l h u | u < l = l
                    | u > h = h
                    | otherwise = u
        closest = V3 (clamp x x' bx) (clamp y y' by) (clamp z z' bz)
    in manhatten b closest <= r

contain :: Cube -> Bot -> Bool
contain c (Bot pb r) = all ((<=r).manhatten pb) $ vertices c

stats :: [Bot] -> Cube -> Bounds
stats bs c = (length $ filter (contain c) bs
             ,length $ filter (intersect c) bs)

split :: Cube -> [Cube]
split (V3 xm ym zm, V3 xM yM zM) = let dx = xM-xm
                                       dy = yM-ym
                                       dz = zM-zm
                                       x2 = (xm+xM) `div` 2
                                       y2 = (ym+yM) `div` 2
                                       z2 = (zm+zM) `div` 2
                                   in if dx == 0 && dy == 0 && dz == 0
                                      then [(V3 xm ym zm, V3 xM yM zM)]
                                      else if dx >= max dy dz
                                           then [(V3 xm ym zm, V3 x2 yM zM), (V3 (x2+1) ym zm, V3 xM yM zM)]
                                           else if dy >= dz
                                                then [(V3 xm ym zm, V3 xM y2 zM), (V3 xm (y2+1) zm, V3 xM yM zM)]
                                                else [(V3 xm ym zm, V3 xM yM z2), (V3 xm ym (z2+1), V3 xM yM zM)]

-- for debugging
size :: Cube -> Integer
size (V3 xm ym zm, V3 xM yM zM) = (fromIntegral $ xM-xm+1)*(fromIntegral $ yM-ym+1)*(fromIntegral $ zM-zm+1)

-- Find all points with maximal coverage, (and also how well covered they are)
day23b_solve' :: [Bot] -> (Int, [Cube])
day23b_solve' bs = let xs = map (_x . _pos) bs
                       ys = map (_y . _pos) bs
                       zs = map (_z . _pos) bs
                       p = V3 (minimum xs) (minimum ys) (minimum zs)
                       q = V3 (maximum xs) (maximum ys) (maximum zs)
                       -- initially: a cube containing all bots
                       -- use Set as a poor-mans priority queue
                   in go (-1) [] $ S.singleton (prio (p,q), (p,q))
  where -- keep track of best minimum so far, and all cubes that have min=max=best
        go best bestCubes search
          = case S.maxView search of
              Nothing -> (best, bestCubes)
              Just (((mx,mn),c),search')
                -> case (compare mn best, compare mx best) of
                     (_, LT) -> go best bestCubes search' -- this cube can be pruned
                     (EQ,EQ) -> go best (c:bestCubes) search' -- this cube is full of good points
                     (GT, _) -> go mn [] $ S.filter ((>=mn).upper.fst) search -- update best, reprocess this cube
                     _ -> go best bestCubes  -- can't deduce anything useful, let's split this cube
                        $ S.union search'
                        $ S.fromList
                        $ filter ((>=best).upper.fst)
                        $ map (\cu -> (prio cu,cu)) $ split c

        upper (u,_) = u
        -- we'll prioritise finding a good maximum, and break ties with the minimum
        -- and keep min and max around as they are nescessary for pruning
        prio c = let (mn,mx) = stats bs c
                 in (mx,mn)

-- Actually want closest point to origin which has maximal coverage
day23b_solve :: [Bot] -> Either Void Int
day23b_solve bots = let cubes = snd $ day23b_solve' bots
                    in Right $ minimum $ map (manhatten (V3 0 0 0)) $ concatMap vertices cubes

day23b_main :: IO ()
day23b_main = generic_main "../data/23a" day23a_parser day23b_solve show
