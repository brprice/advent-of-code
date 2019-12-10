module Main where

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

main :: IO ()
main = do dat' <- lines <$> readFile "../data/day10"
          let dat = concatMap (\(y,l) -> zipWith (\x c -> ((x,y),c)) [0..] l) $ zip [0..] dat'
          let (maxx,maxy) = fst $ last dat
          let asts' = map fst $ filter (\(_,c) -> c =='#') dat
          let asts = Ast maxx maxy $ S.fromList asts'
          let visibilities = flip map asts' $ \ast -> map (look asts ast) $ takeWhile (\(lx,ly) -> max (abs lx) (abs ly) <= max maxx maxy) sightlines

          putStr "part a: "
          print $ maximum $ map (length . catMaybes) visibilities
