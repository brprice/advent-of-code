module Main where

import Control.Monad.State (evalState)
import Data.Char (chr,ord)
import Data.Foldable (toList)
import Data.List (intersperse, stripPrefix)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Sequence (Seq((:|>)))
import qualified Data.Sequence as Q

import Intcode

-- We don't RLE strings of forward steps until we convert to movement function.
-- This way, our compressor can split a straight if it is useful to.
data Step = L | R | F deriving (Eq,Show)

followPath :: S.Set (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer) -> [Step]
followPath scaffold robot drobot = go (S.delete robot scaffold) robot drobot
  where deleteUnlessIntersection (x,y) set
          | 3 <= length (filter (flip S.member set) [(x+1,y),(x-1,y),(x,y+1),(x,y-1)])
          = set
          | otherwise = S.delete (x,y) set
        go scaf (x,y) (dx,dy)
          | S.null scaf = []
          | otherwise = let f = (x+dx, y+dy) -- move forward
                            (ldx,ldy) = (dy,-dx) -- turn left
                            (rdx,rdy) = (-dy,dx) -- turn right
                        in case () of
                             _ | S.member f scaf -> F : go (deleteUnlessIntersection f scaf) f (dx,dy)
                             _ | S.member (x+ldx , y+ldy) scaf -> L : go scaf (x,y) (ldx,ldy)
                             _ | S.member (x+rdx , y+rdy) scaf -> R : go scaf (x,y) (rdx,rdy)
                             _ -> error "I got stuck trying to find a path"

toFn :: [Step] -> String
toFn = go
  where xs <-> [] = xs
        xs <-> ys = xs ++ ',' : ys
        go [] = []
        go (L:steps) = "L" <-> go steps
        go (R:steps) = "R" <-> go steps
        go steps = let (fs,rest) = span (==F) steps
                   in show (length fs) <-> go rest

{-
Compression, for part b
This is a depth-first search, where the children of a state are
  - match a prefix of the string against an entry in the dictionary
    (for those "good" prefixes, given by a parameter)
  - add a new dictionary entry for some prefix of the string
    (if the dictionary has room to grow)
In both cases, we consume that prefix.
We also enforce bounds on the length of the compressed string.
-}

data Compressed a = Comp {_dict :: Seq [a]
                         ,_comp :: Seq Int -- indexes into dict
                         } deriving Show
compressStep :: Eq a => Int -> ([a] -> Bool) -> (Compressed a,[a]) -> [(Compressed a,[a])]
compressStep dictSize prefixP (Comp dict comp, rest) = addPrefix ++ match
  where addPrefix | length dict < dictSize
                  = map (\(xs,ys) -> (Comp (dict:|>xs) (comp :|> length dict),ys)) $ takeWhile (prefixP.fst) $ splits rest
                  | otherwise = mempty
        splits [] = [([],[])]
        splits (x:xs) = ([],x:xs) : map (\(h,t) -> ((x:h),t)) (splits xs)
        match = mapMaybe (\(i,di) -> (\rest' -> (Comp dict (comp :|> i),rest')) <$> stripPrefix di rest) $ zip [0..] (toList dict)

compress :: Eq a => Int -> Int -> ([a] -> Bool) -> [a] -> [Compressed a]
compress dictSize maxCompLen prefixP s = go [(Comp Q.empty Q.empty,s)]
  where go [] = []
        go ((c,[]):xs) = c : go xs
        go (x:xs) = let new = filter ((<=maxCompLen) . Q.length . _comp . fst) $ compressStep dictSize prefixP x
                    in go (new ++ xs)

main :: IO ()
main = do robot <- readIntcode "../data/day17"
          let scaffold = lines $ map (chr.fromInteger) $ evalState (run []) robot
          let scafXY = concat $ zipWith (\y l -> zipWith (\x c -> ((x,y),c)) [0..] l) [0..] scaffold
          let scafL = map fst $ filter (flip elem ['#','^','>','<','v'].snd) scafXY
          let scafS = S.fromList scafL
          let intersections = filter (\(x,y) -> all (\(dx,dy) -> (x+dx,y+dy) `S.member` scafS) [(-1,0),(1,0),(0,-1),(0,1)]) scafL
          let parta :: Integer
              parta = sum $ map (uncurry (*)) intersections
          putStr "part a: "
          print parta

          -- Part b
          -- first, find the path
          -- We choose to go forward wherever possible, and hope that that path will compress enough
          let (robotPos,robotDir) = case filter (flip elem ['^','>','<','v'].snd) scafXY of
                [((x,y),'^')] -> ((x,y),(0,-1))
                [((x,y),'v')] -> ((x,y),(0, 1))
                [((x,y),'>')] -> ((x,y),( 1,0))
                [((x,y),'<')] -> ((x,y),(-1,0))
                _ -> error "failed to find robot"
          let path = followPath scafS robotPos robotDir

          -- second, compress it
          let dictSize = 3
          let maxLen = 20
          let maxCompLen = 10 -- 10 calls to functions expands to a main routine of length 19 when commas are added
          let (comp:_) = compress dictSize maxCompLen ((<=maxLen).length.toFn) path
          let (fa':fb':fc':_) = toList (_dict comp) ++ repeat []
          let [fa,fb,fc] = map toFn [fa',fb',fc']
          let fmain = intersperse ',' $ map ("ABC"!!) $ toList $ _comp comp

          -- finally, run it
          let robotB = robot{mem = M.insert 0 2 $ mem robot}
          let input = map (fromIntegral.ord) $ unlines [fmain,fa,fb,fc,"n"] -- "n" for no video feed
          let out = evalState (run input) robotB
          putStr "part b: "
          print $ last out
