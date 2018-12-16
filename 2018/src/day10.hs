import Debug.Trace

import Control.DeepSeq

import qualified Data.Set as S
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Util

-- 10a: given a set of points and linear motion, wait some time until the
-- points spell out a message

-- either a ' ' or '-' preceeding an integer
number :: Parser Int
number = string " " *> decimal <|> negate <$ string "-" <*> decimal

parse_named_pair :: String -> Parser (Int,Int)
parse_named_pair n = (,) <$ string n <* string "=<" <*> number <* string ", " <*> number <* string ">"

type Point = (Int,Int)
type Velocity = (Int,Int)

day10a_parser :: Parser [(Point, Velocity)]
day10a_parser = many $ (,) <$> parse_named_pair "position" <*> parse_named_pair " velocity" <* eol

shorterThan :: [a] -> Int -> Bool
shorterThan xs 0 = False
shorterThan [] _ = True
shorterThan (x:xs) n = shorterThan xs (n-1)

-- 70% of points are in a line at least 3 long
aligned :: Int -> [Point] -> Bool
aligned thresh ps' = let ps = S.fromList ps'
                         inLineX (x,y) = case ((x-2,y)`elem`ps,(x-1,y)`elem`ps,(x+1,y)`elem`ps,(x+2,y)`elem`ps)
                                         of (True,True,_,_) -> True
                                            (_,True,True,_) -> True
                                            (_,_,True,True) -> True
                                            _ -> False
                         inLineY (x,y) = case ((x,y-2)`elem`ps,(x,y-1)`elem`ps,(x,y+1)`elem`ps,(x,y+2)`elem`ps)
                                         of (True,True,_,_) -> True
                                            (_,True,True,_) -> True
                                            (_,_,True,True) -> True
                                            _ -> False
                         inLine p = inLineX p || inLineY p
                         unLined = filter (not.inLine) ps'
                     in unLined `shorterThan` thresh

day10a_solve :: [(Point, Velocity)] -> Either Void [Point]
day10a_solve start = pure $ head $ filter (aligned (floor $ 0.7*fromIntegral (length start))) $ map (map fst) steps
  where steps = map snd $ iterate (step.force) (0,start)
        step (s,pvs) | trace (show s) False = undefined
                     | True = (s+(1::Int),map (\((x,y),(dx,dy)) -> ((x+dx,y+dy),(dx,dy))) pvs)

bb ::  [Point] -> ((Int,Int),(Int,Int))
bb ((x,y):ps) = let (l,r,t,b) = foldr (\(x,y) (l,r,t,b) -> (min x l, max x r, min y t, max y b))
                                          (x,x,y,y)
                                          ps
                    in ((l,t),(r,b))

day10a_show :: [Point] -> String
day10a_show ps = let ((l,t),(r,b)) = bb ps
                     f (x,y) = if (x,y) `elem` ps
                               then '#'
                               else ' '
                 in unlines [[f (x,y) | x<-[l..r]] | y<-[t..b]]

day10a_main :: IO ()
day10a_main = generic_main "../data/10a" day10a_parser day10a_solve day10a_show

main = day10a_main

-- 10b: how long does it take to get the message
-- solution: times are alread printed in part a
