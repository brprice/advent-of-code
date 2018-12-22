{-# LANGUAGE LambdaCase #-}

import Control.Lens

import Data.Bifunctor
import Data.Function
import Data.Maybe
import Data.List

import qualified Data.Map as M
import qualified Data.Set as S

-- 15a: simulate a roguelike combat

data V2 = V2 {_x, _y :: Int} deriving (Eq, Show)
instance Ord V2 where
  compare (V2 x y) (V2 x' y') = compare (y,x) (y',x')

-- Let's hand roll a lens, for practice
x, y :: Lens' V2 Int
x f (V2 x y) = (\x' -> V2 x' y) <$> f x
y f (V2 x y) = (\y' -> V2 x y') <$> f y

type Graph = M.Map V2 [V2] -- map of outgoing edges
data Unit = Goblin | Elf deriving (Eq, Ord, Show)
type Units = M.Map V2 (Unit, Int)

orthog :: V2 -> [V2]
orthog p = [p & x +~ 1
           ,p & x -~ 1
           ,p & y +~ 1
           ,p & y -~ 1
           ]

-- Static graph of open spaces, overlayed with a dynamic set of units,
-- who block movement.
day15a_parser :: String -> (Graph, Units)
day15a_parser s = (M.fromList graph
                  , M.fromList $ map (\(p,u) -> (p,(u,health))) units)
  where health = 200
        spos = concat
             $ zipWith (\y xs -> map (\(x,c) -> (V2 x y,c)) xs) [0..]
             $ map (zip [0..])
             $ lines s
        open = map fst $ filter (\(p,c) -> c`elem`"EG.") spos
        openS = S.fromList open
        units = map (second (\case 'G' -> Goblin; 'E' -> Elf))
              $ filter (\(p,c) -> c`elem`"EG") spos
        graph = map (\p -> (p,[p' | p'<-orthog p, S.member p' openS])) open

race :: (Unit, Int) -> Unit
race (u,_) = u

-- is the square open and unoccupied?
avail :: Graph -> Units -> V2 -> Bool
avail g us p = M.member p g && not (M.member p us)

topLeft :: [V2] -> Maybe V2
topLeft vs = sort vs ^? _head

data Inf a = Fin a | Inf deriving (Eq, Ord, Show)

(+.) :: Num a => Inf a -> Inf a -> Inf a
Inf +. _ = Inf
_ +. Inf = Inf
Fin a +. Fin b = Fin $ a + b

dijkstra :: Graph -> V2 -> M.Map V2 (Inf Int)
dijkstra g p = let dists = M.insert p (Fin 0)
                         $ M.map (const Inf) g
                   toVisit = S.fromList
                           $ map swap
                           $ M.assocs dists
               in go dists toVisit
  where swap (a,b) = (b,a)
        go ds todo = case S.minView todo of
                  Nothing -> ds -- done!
                  Just ((d,p),todo') ->
                    let oldDists = map (\p' -> (p',ds M.! p')) (g M.! p)
                        oldDistsTodo = filter (`S.member` todo') (map swap oldDists)
                        newDists = map (\(p',d') -> (p',min d' $ d+.Fin 1)) oldDists
                        newDistsTodo = map (\(d',p') -> (min d' $ d+.Fin 1,p')) oldDistsTodo
                        todo'' = S.union (S.fromList newDistsTodo)
                               $ S.difference todo' (S.fromList oldDistsTodo)
                    in go (M.union (M.fromList newDists) ds) todo''


minimumsOn :: Ord b => (a -> b) -> [a] -> [a]
minimumsOn f = (\case [] -> []; (g:gs) -> g) . groupBy ((==)`on`f) . sortBy (compare`on`f)

obscure :: Graph -> Units -> Graph
obscure g us = M.map (\ns -> filter (not.flip M.member us) ns) $ M.filterWithKey (\p _ -> not $ M.member p us) g

-- Run Dijkstra once for each first step, find the best.
-- Also, consider not moving at all!
-- We don't need to modify the graph for different first steps,
-- since the best path will not cross back to the original position
move :: Graph -> Units -> Units -> V2 -> Maybe V2
move g us targs p = let targetCells = filter (\p' -> p'==p || avail g us p')
                                    $ concatMap orthog
                                    $ M.keys targs
                        g' = obscure g us
                        firstSteps = filter (avail g us) $ orthog p
                        minPath from = minimum $ map (dijkstra g' from M.!) targetCells
                        minPaths = map (\f -> (minPath f,f)) firstSteps
                        bestStep' = minimumsOn fst minPaths
                        len = fst $ head bestStep'
                        bestStep = map snd bestStep'
                    in if null targetCells || p `elem` targetCells || len == Inf
                       then Nothing -- Don't move
                       else topLeft bestStep

-- May return a dead enemy, as 0 or neg health
-- needs to be updated in unit map later
attack :: Graph -> Units -> V2 -> Int -> Maybe (V2, (Unit, Int))
attack g targets p atkPow = let adjacent = filter (\p' -> M.member p' targets)
                                         $ orthog p
                                weakestAdjacent = minimumsOn (snd . (M.!) targets)
                                                             adjacent
                            in do e <- topLeft weakestAdjacent -- attack
                                  pure $ (e, second (subtract atkPow)
                                       $ targets M.! e)

changeKey :: Ord k => k -> k -> M.Map k v -> M.Map k v
changeKey old new us = M.insert new (us M.! old) $ M.delete old us


-- bool is whether turn completed (True) or finished early
turn :: Graph -> (Unit -> Int) -> Units -> (V2, Unit) -> (Units, Bool)
turn g atkPows us (p,u) = let targets = M.filter ((/=u).race) us
                              moveTo = move g us targets p
                              p' = fromMaybe p moveTo
                              moved = changeKey p p' us
                          in if M.null targets
                             then (us, False) -- no targets, combat ends
                             else case attack g targets p' (atkPows u)
                                  of Nothing -> (moved, True)
                                     Just (p'',(u'',h'')) ->
                                       if h''<=0
                                       then (M.delete p'' moved, True)
                                       else (M.insert p'' (u'',h'') moved, True)

-- bool is whether round completed (True) or finished early
doRound :: Graph -> (Unit -> Int) -> Units -> (Units , Bool)
doRound g atkPows us = go us (map (\(p,(u,_)) -> (p,u)) $ M.assocs us)
  where go us [] = (us,True)
        go us ((p,u):todos) | p `M.member` us = case turn g atkPows us (p,u) of
                                                  (us,False) -> (us,False)
                                                  (us,True) -> go us todos
                            | otherwise = go us todos -- unit has died

day15a_solve :: (Graph, Units) -> Int
day15a_solve (g, us) = let r = doRound g (const 3) -- all units have attack 3
                           rounds = zip [0..]
                                  $ iterate (r . fst) (us, True)
                           (n,(us',_)) = head $ dropWhile (snd.snd) rounds
                           hps = M.map (\(_,h) -> h) us'
                       in (n-1) * sum hps


day15a_main :: IO ()
day15a_main = print . day15a_solve . day15a_parser =<< readFile "../data/15a"


-- 15b: Buff Elves' attack power until none of them die
day15b_solve :: (Graph, Units) -> Int
day15b_solve (g, us) = incElfAttack 4
  where numElves = M.size . M.filter ((==Elf).race)
        origElves = numElves us
        incElfAttack k = let r = doRound g (\case Goblin -> 3; Elf -> k)
                             rounds = iterate (r . fst) (us, True)
                       in case go 0 rounds of
                            Nothing -> incElfAttack (k+1)
                            Just (n,us) -> (n-1) * sum (M.map (\(_,h) -> h) us)
        go n ((us,b):usbs) | numElves us /= origElves = Nothing
                           | b = go (n+1) usbs
                           | otherwise = Just (n,us)


day15b_main :: IO ()
day15b_main = print . day15b_solve . day15a_parser =<< readFile "../data/15a"


main :: IO ()
main = day15b_main
