import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Void

import Util

data V2 = V2 {_x, _y :: Int} deriving (Show, Eq, Ord)
data Terrain = Open | Trees | Lumberyard deriving (Show, Eq, Ord)

(+.) :: V2 -> V2 -> V2
V2 a b +. V2 c d = V2 (a+c) (b+d)

-- 18a: run a 2d cellular automata for 10 steps
day18a_parser :: Parser (M.Map V2 Terrain)
day18a_parser = toMap <$> many (line <* eol)
  where line = many cell
        cell = Open <$ string "."
           <|> Trees <$ string "|"
           <|> Lumberyard <$ string "#"
        toMap tss = M.fromList
                  $ concat
                  $ zipWith (\y ts -> zipWith (\x t -> (V2 x y,t)) [0..] ts)
                            [0..]
                            tss

showMap :: M.Map V2 Terrain -> String
showMap ts = let V2 minx miny = fst $ M.findMin ts
                 V2 maxx maxy = fst $ M.findMax ts
             in unlines [[cell $ V2 x y | x<-[minx..maxx]] | y<-[miny..maxy]]
  where cell p = cell' $ ts M.! p
        cell' Open = '.'
        cell' Trees = '|'
        cell' Lumberyard = '#'

nbd :: V2 -> [V2]
nbd p = map (p+.) [V2 x y | x<-[-1,0,1], y<-[-1,0,1], not (x == 0 && y == 0)]

-- the cell and its (up to) 8 surrounding squares
context :: M.Map V2 Terrain -> M.Map V2 (Terrain,[Terrain])
context ts = M.mapWithKey (\p t -> (t, go p)) ts
  where go p = catMaybes $ map (flip M.lookup ts) $ nbd p

tick' :: Terrain -> [Terrain] -> Terrain
tick' Open adj = if (>=3) $ length $ filter (==Trees) adj
                 then Trees
                 else Open
tick' Trees adj = if (>=3) $ length $ filter (==Lumberyard) adj
                  then Lumberyard
                  else Trees
tick' Lumberyard adj = if Lumberyard`elem`adj && Trees`elem`adj
                       then Lumberyard
                       else Open

tick :: M.Map V2 Terrain -> M.Map V2 Terrain
tick = M.map (uncurry tick') . context

day18a_solve :: M.Map V2 Terrain -> Either Void Int
day18a_solve ts = let fin = iterate tick ts !! 10
                      trees = M.size $ M.filter (==Trees) fin
                      lumber = M.size $ M.filter (==Lumberyard) fin
                  in pure $ trees * lumber

day18a_main :: IO ()
day18a_main = generic_main "../data/18a" day18a_parser day18a_solve show


-- 18b: now for 1000000000 steps

findRepeat :: Ord a => [a] -> (Int,Int)
findRepeat xs = go M.empty xs 0
  where go _ [] _ = error "no repeat!"
        go seen (y:ys) n = case M.lookup y seen of
                             Nothing -> go (M.insert y n seen) ys (n+1)
                             Just m -> (m,n-m)

day18b_solve :: M.Map V2 Terrain -> Either Void Int
day18b_solve ts = let tss = iterate tick ts
                      (first,period) = findRepeat tss
                      target = 1000000000
                      r = (target-first) `rem` period
                      fin = tss !! (first + r)
                      trees = M.size $ M.filter (==Trees) fin
                      lumber = M.size $ M.filter (==Lumberyard) fin
                  in pure $ trees * lumber

day18b_main :: IO ()
day18b_main = generic_main "../data/18a" day18a_parser day18b_solve show

main :: IO()
main = do Right ts <- runParser day18a_parser "" <$> readFile "../data/18a"
          mapM_ (\(n,m) -> putStr "\nn=" >> print n >> putStrLn (showMap m)) $ zip [0::Int ..] $ iterate tick ts
