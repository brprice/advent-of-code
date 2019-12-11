{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S

import Intcode

newtype V2 = V2 {unV2 :: (Int,Int)}
  deriving (Eq,Ord)
makePrisms ''V2

data Dir = Up | Rgt | Down | Lft deriving Enum
data Colour = Black | White deriving (Enum, Eq)

right :: Dir -> Dir
right = toEnum . flip mod 4 . (+1) . fromEnum

left :: Dir -> Dir
left = toEnum . flip mod 4 . (\x -> x-1) . fromEnum

data RobotShip = RobotShip {_pos :: V2, _dir :: Dir, _paint :: M.Map V2 Colour}
makeLenses ''RobotShip

forward :: RobotShip -> RobotShip
forward ship = ship & pos %~ f (ship ^. dir)
  where f Up p = p & _V2 . _2 %~ (\x -> x-1)
        f Down p = p & _V2 . _2 %~ (+1)
        f Lft p = p & _V2 . _1 %~ (\x -> x-1)
        f Rgt p = p & _V2 . _1 %~ (+1)

runRobot :: IC -> RobotShip -> RobotShip
runRobot = go True
  where go painting ic ship = case runToIO ic of
          ICIn f -> go painting (f $ ship ^. paint . at (ship ^. pos) . to (fromIntegral . maybe 0 fromEnum)) ship
          ICOut o ic' -> let ship' = if painting
                                     then ship & paint . at (ship ^. pos) .~ Just (toEnum $ fromInteger o)
                                     else forward (ship & dir %~ if o == 0 then left else right)
                         in go (not painting) ic' ship'
          ICHalt _ -> ship

main :: IO ()
main = do mach <- readIntcode "../data/day11"
          let shipa = runRobot mach $ RobotShip (V2 (0,0)) Up M.empty
          putStr "part a: "
          print $ shipa ^. paint . to M.size

          let shipb = runRobot mach $ RobotShip (V2 (0,0)) Up (M.singleton (V2 (0,0)) White)
          let whites = findIndicesOf (paint . ifolded) (==White) shipb
          let xmin = minimum1Of (traverse._V2._1) whites
          let xmax = maximum1Of (traverse._V2._1) whites
          let ymin = minimum1Of (traverse._V2._2) whites
          let ymax = maximum1Of (traverse._V2._2) whites
          let whites' = S.fromList whites
          putStrLn "part b:"
          mapM_ putStrLn [[if S.member (V2 (x,y)) whites' then '#' else ' '  | x<-[xmin..xmax]]| y<-[ymin..ymax]]
