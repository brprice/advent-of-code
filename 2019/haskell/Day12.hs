{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Lens
import Data.List (genericLength)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

data V3 = V3{_x,_y,_z::Integer}
makeLenses ''V3

(.+) :: V3 -> V3 -> V3
(V3 a b c) .+ (V3 d e f) = V3 (a+d) (b+e) (c+f)

data Planet = Planet{_pos,_vel::V3}
makeLenses ''Planet

{- input is of form
<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>
-}
parser :: Parsec Void String [Planet]
parser = many planet <* eof
  where v3 = V3 <$ string "<x=" <*> num <* string ", y=" <*> num <* string ", z=" <*> num <* string ">" <* eol
        planet = (\p -> Planet p (V3 0 0 0)) <$> v3
        num = signed (pure ()) decimal

main :: IO()
main = do let dat = "../data/day12"
          input <- readFile dat
          let parsed = parse parser dat input
          case parsed of
            Left err -> print err
            Right planets -> do
              putStr "part a: "
              print $ parta planets
              putStr "part b: "
              print $ partb planets

parta :: [Planet] -> Integer
parta planets = totalEnergy $ iterate step planets !! 1000

step :: [Planet] -> [Planet]
step ps = ps & each %~ step1
  where step1 :: Planet -> Planet
        step1 p = let p' = p & vel %~ (.+ (foldrOf (folded.to (dir p)) (.+) (V3 0 0 0) ps))
                  in p' & pos %~ (.+ (p' ^. vel))
        dir p q = V3 (cmp (p^.pos.x) (q^.pos.x)) (cmp (p^.pos.y) (q^.pos.y)) (cmp (p^.pos.z) (q^.pos.z))
        cmp a b = case compare a b of
                    LT -> 1
                    EQ -> 0
                    GT -> -1

totalEnergy :: [Planet] -> Integer
totalEnergy ps = ps & sumOf (each.to tot)
  where tot p = (p ^. pos . to l1) * (p ^. vel . to l1)
        l1 = sumOf (x.to abs<>y.to abs<>z.to abs)


{-
Part b needs a bit of thought. There are two insights required here:
Firstly: the process is reversable, so the first repeated state is the start state;
Secondly: the three dimensions are independent.
Thus we iterate until we have seed a repitition in each three dimensions individually,
and then take the lcm of those times.
-}
partb :: [Planet] -> Integer
partb planets = let future = tail $ iterate step planets
                    getDim :: Lens' V3 a -> [Planet] -> ([a],[a])
                    getDim d ps = (ps^..each.pos.d, ps^..each.vel.d)
                    rep :: Eq a => Lens' V3 a -> Integer
                    rep d = succ $ genericLength $ takeWhile (\fps -> not $ getDim d planets == getDim d fps) $ future
                in foldr lcm 1 $ [rep x, rep y, rep z]
