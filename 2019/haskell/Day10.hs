module Main where

import Data.List (mapAccumL, sortOn)
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Set as S

data Asteriods = Ast {mx,my :: Int, asteroids :: S.Set (Int,Int)}

inField :: Asteriods -> (Int,Int) -> Bool
inField fld (x,y) = 0 <= x && 0 <= y && x <= mx fld && y <= my fld

-- ordered by l^infinity distance to first lattice point on the line
sightlines :: [(Int,Int)]
sightlines = (0,1):(0,-1):(1,0):(-1,0): rest
  where rest = concatMap f $ (1,1) : concat [concat [[(x,y),(y,x)] | y<- filter (coprime x) [1..x-1]] | x<-[1..]]
        f (x,y) = [(x,y),(x,-y),(-x,y),(-x,-y)]
        coprime x y = gcd x y == 1

(.+) :: (Int,Int) -> (Int,Int) -> (Int,Int)
(a,b) .+ (c,d) = (a+c,b+d)
infixl 6 .+

(.*) :: Int -> (Int,Int) -> (Int,Int)
k .* (a,b) = (k*a,k*b)
infix 7 .*

look :: Asteriods -> (Int,Int) -> (Int,Int) -> Maybe (Int,Int)
look asts ast l = listToMaybe $ filter (flip S.member (asteroids asts)) $ takeWhile (inField asts) $ map (\k -> ast.+k.*l) [1..]

{- part b: we now want to scan clockwise, so sort sightlines as (offsets)
(0,-1),(1,-inf..0),(2,-inf..0)...(1,0),
We do this by calculating their angle in (-pi,pi] from "up"
-}
angle :: (Int,Int) -> Double
{- NB: positive input direction is right and down, we want to order as "up" before "right"
before "down" before "left".
atan2 y x  assumes positive is right and up, and gives clockwise angle from x->+inf, in range (-pi,pi]
atan2 (-x) y, not atan2 y x to measure clockwise from "down", with the branch cut "upwards"
-}
angle (x, y) = atan2 (negate $ fromIntegral x) (fromIntegral y)

vaporise :: Asteriods -> (Int,Int) -> Asteriods
vaporise asts p = asts{asteroids = S.delete p $ asteroids asts}

main :: IO ()
main = do dat' <- lines <$> readFile "../data/day10"
          let dat = concatMap (\(y,l) -> zipWith (\x c -> ((x,y),c)) [0..] l) $ zip [0..] dat'
          let (maxx,maxy) = fst $ last dat
          let asts' = map fst $ filter (\(_,c) -> c =='#') dat
          let asts = Ast maxx maxy $ S.fromList asts'
          let sightlinesBounded = takeWhile (\(lx,ly) -> max (abs lx) (abs ly) <= max maxx maxy) sightlines
          let visibilities = flip map asts' $ \ast -> map (look asts ast) sightlinesBounded
          let (mostVisCount,mostVis) = maximum $ zip (map (length . catMaybes) visibilities) asts'

          putStr "part a: "
          print mostVisCount

          let sightClock = cycle $ sortOn angle sightlinesBounded
          let vaporised = snd $ mapAccumL (\astsLeft l -> maybe (asts,Nothing) (\v -> (vaporise astsLeft v,Just v)) $ look asts mostVis l) asts sightClock
          let betsOn = 200
          let luckyAsteroid = catMaybes vaporised !! (betsOn - 1)
          putStr "part b: "
          print $ 100*fst luckyAsteroid+snd luckyAsteroid
