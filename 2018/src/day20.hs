{-# LANGUAGE LambdaCase #-}

import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Text.Megaparsec
import Text.Megaparsec.Char

import Util

data Regex a = Letter a | Cat [Regex a] | Alt [Regex a] deriving Show
data V2 = V2 {_x, _y :: Int} deriving (Eq, Ord, Show)
type Graph = M.Map V2 (S.Set V2)

(+.) :: V2 -> V2 -> V2
V2 a b +. V2 c d = V2 (a+c) (b+d)

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

-- 20a: Regex maze thing
-- How far away is the furthest away room (always counting shortest paths)
day20a_parser :: Parser (Regex V2)
day20a_parser = string "^" *> regex <* string "$" <* eol
  where regex = --(Cat <$> regex1 <*> regex1)
            {-<|>-} (\case [x] -> x ; xs -> Cat xs) <$> many regex1
            -- <|> pure Eps
        regex1 = ((\r rs -> Alt (r:rs)) <$ string "(" <*> regex <*> many (string "|" *> regex) <* string ")")
             <|> (Letter <$> dir)
        dir = V2 1 0 <$ string "E"
          <|> V2 (-1) 0 <$ string "W"
          <|> V2 0 (-1) <$ string "N"
          <|> V2 0 1 <$ string "S"

newtype UPair a = UP (a,a) deriving Show
sndU :: UPair a -> a
sndU (UP p) = snd p
canon :: Ord a => UPair a -> (a,a)
canon (UP (x,y)) | x <= y = (x,y)
                 | otherwise = (y,x)
instance Ord a => Eq (UPair a) where
  p == q = canon p == canon q
instance Ord a => Ord (UPair a) where
  compare p q = compare (canon p) (canon q)

regexToDoors :: Regex V2 -> S.Set (UPair V2)
regexToDoors = snd . go (S.singleton (V2 0 0) , S.empty)
  where go :: (S.Set V2,S.Set (UPair V2)) -> Regex V2 -> (S.Set V2,S.Set (UPair V2))
        go (locs, ds) (Letter d) = let ds' = S.map (\l -> UP (l, l+.d)) locs
                                   in (S.map sndU ds', S.union ds ds')
        go lds (Cat ps) = foldl' go lds ps
        go lds (Alt ps) = let (locs', doors') = unzip $ map (go lds) ps
                          in (S.unions locs', S.unions doors')

doorsToGraph :: S.Set (UPair V2) -> Graph
doorsToGraph = S.foldr conn M.empty
  where conn (UP (p,q)) doors = M.unionWith (<>) doors
                              $ M.fromList [(p,S.singleton q),(q,S.singleton p)]

showGraph :: Graph -> String
showGraph g = let keys = M.keys g
                  top = minimum $ 0 : map _y keys
                  bot = maximum $ map _y keys
                  left = minimum $ map _x keys
                  right = maximum $ map _x keys
                  cell x y = case (x`divMod`2,y`divMod`2) of
                               ((0,0),(0,0)) -> 'X' -- start location
                               ((_,0),(_,0)) -> '.' -- middle of room
                               ((x2,0),(y2,1)) -> door (V2 x2 y2) (V2 x2 (y2+1)) -- between rooms (x2,y2) and (x2,y2+1)
                               ((x2,1),(y2,0)) -> door (V2 x2 y2) (V2 (x2+1) y2) -- between rooms (x2,y2) and (x2+1,y2)
                               ((_,1),(_,1)) -> '#' -- wall in corner of 4 rooms
                               _ -> error "Impossible, cases are actually exhaustive"
                  door p q = maybe '#' (\nbd -> if q`elem`nbd then '+' else '#') $ M.lookup p g
              in unlines [[cell x y | x <- [left*2-1..right*2+1]] | y <- [top*2-1..bot*2+1]]


-- dijkstra and Inf copied from day 15
data Inf a = Fin a | Inf deriving (Eq, Ord, Show)

(+^) :: Num a => Inf a -> Inf a -> Inf a
Inf +^ _ = Inf
_ +^ Inf = Inf
Fin a +^ Fin b = Fin $ a + b

dijkstra :: Graph -> V2 -> M.Map V2 (Inf Int)
dijkstra g q = let dists = M.insert q (Fin 0)
                         $ M.map (const Inf) g
                   toVisit = S.fromList
                           $ map swap
                           $ M.assocs dists
               in go dists toVisit
  where swap (a,b) = (b,a)
        go ds todo = case S.minView todo of
                  Nothing -> ds -- done!
                  Just ((d,p),todo') ->
                    let oldDists = map (\p' -> (p',ds M.! p')) $ S.toList $ g M.! p
                        oldDistsTodo = filter (`S.member` todo') (map swap oldDists)
                        newDists = map (\(p',d') -> (p',min d' $ d+^Fin 1)) oldDists
                        newDistsTodo = map (\(d',p') -> (min d' $ d+^Fin 1,p')) oldDistsTodo
                        todo'' = S.union (S.fromList newDistsTodo)
                               $ S.difference todo' (S.fromList oldDistsTodo)
                    in go (M.union (M.fromList newDists) ds) todo''


day20a_solve :: Regex V2 -> Either String Int
day20a_solve r = let g = doorsToGraph $ regexToDoors r
                     dists = dijkstra g $ V2 0 0
                     furthest = maximum $ M.elems dists
                 in case furthest of
                      Inf -> Left "How is the graph disconnected?"
                      Fin n -> Right n
day20a_main :: IO ()
day20a_main = generic_main "../data/20a" day20a_parser day20a_solve show


-- 20b: How many rooms are at least 1000 doors away (along shortest paths)
day20b_solve :: Regex V2 -> Either String Int
day20b_solve r = let g = doorsToGraph $ regexToDoors r
                     dists = dijkstra g $ V2 0 0
                     far = filter (>=Fin 1000) $ M.elems dists
                     allFin = not $ any (==Inf) dists
                 in if allFin
                    then Right $ length far
                    else Left "How is graph disconnected?"

day20b_main :: IO ()
day20b_main = generic_main "../data/20a" day20a_parser day20b_solve show
