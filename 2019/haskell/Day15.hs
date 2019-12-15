module Main where

import Control.Lens
import qualified Data.Map as M
import qualified Data.Sequence as Q
import qualified Data.Set as S

import Intcode

type Pos = (Integer,Integer)
type Graph a = M.Map Pos (a,[Pos])
data Dir = N | S | W | E

data MoveResult = Wall | Move Bool Robot -- True means contains system
type Robot = Dir -> MoveResult

move :: Dir -> Pos -> Pos
move N p = p & _1 +~ 1
move S p = p & _1 -~ 1
move W p = p & _2 +~ 1
move E p = p & _2 -~ 1

{-
Assume the state 'a' is confluent-up-to-equivalence.
By this we mean:
Given the unfolding function f : (Pos,a) -> Set (Pos,a)
(the implementation uses lists, but we do not care about order)
Define "s and s' are equivalent at x" by
  s ~x~ s' = map fst (f (x,s)) == map fst (f (x,s'))
         && whenever (y,t) is in f (x,s) and (y,t') is in f (x,s')
            then t ~y~ t'
We require that for any two states for the same position we find by repeated applications
of the unfolding function, that the states are equivalent for that position.
I.e. the neighbours of a location are essentially determined by the location,
and don't differ with the state (although that is needed to compute them).
Thus the search strategy does not matter.
-}
unfoldGraph :: ((Pos,a) -> [(Pos,a,b)]) -> (Pos,a,b) -> Graph b
unfoldGraph f = go M.empty . (:[])
  where go g [] = g
        go g ((p,a,b):todo) | M.member p g = go g todo
                            | otherwise = let childs = f (p,a)
                                          in go (M.insert p (b,childs^..each._1) g) $ childs ++ todo

takeStep :: (Pos,Robot) -> Dir -> Maybe (Pos,Robot,Bool)
takeStep (l,r) d = case r d of
  Wall -> Nothing
  Move sys r' -> Just (move d l, r', sys)

mkRobot :: IC -> Robot
mkRobot ic = case runToIO ic of
  ICIn f -> \d -> case runToIO $ f $ toInput d of
    ICOut o ic' -> case o of
      0 -> Wall
      1 -> Move False $ mkRobot ic'
      2 -> Move True $ mkRobot ic'
      _ -> error "mkRobot: output not in {0,1,2}"
    _  -> error "mkRobot: does not have 1-input-1-output-forever loop A"
  _ -> error "mkRobot: does not have 1-input-1-output-forever loop B"
  where toInput N = 1
        toInput S = 2
        toInput W = 3
        toInput E = 4

-- finds location and path length from start
bfs :: ((Pos,a) -> Bool) -> Pos -> Graph a -> Maybe (Pos,Integer)
bfs p start g = go S.empty $ Q.singleton (start,0)
  where go _ Q.Empty = Nothing
        go seen ((l,d) Q.:<| rest)
          | S.member l seen = go seen rest
          | otherwise = let (a,ch) = g M.! l
                        in if p (l,a)
                             then Just (l,d)
                             else go (S.insert l seen) $ rest Q.>< Q.fromList (map (\c -> (c,d+1)) ch)

main :: IO ()
main = do roboBrains <- readIntcode "../data/day15"
          let robot = mkRobot roboBrains
          let area = unfoldGraph (\x -> [N,S,E,W] ^.. each . to (takeStep x) . _Just) ((0,0),robot,False)
          -- part a: we map the whole space, and then do a breadth-first search for the target
          let Just (loc,dist) = bfs (\(_,system) -> system) (0,0) area
          putStrLn $ unwords ["part a: O2 system is at",show loc,"min steps is",show dist]
