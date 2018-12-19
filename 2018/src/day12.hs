{-# LANGUAGE DeriveFunctor #-}
import Control.Comonad
import Data.Void

import Text.Megaparsec hiding (Stream)
import Text.Megaparsec.Char

import Util

-- 12a: run a given cellular automaton for 20 generations


data Bwd a = Snoc (Bwd a) a
  deriving Functor
data BwdL a = Nil | SnocL (BwdL a) a
  deriving (Eq,Functor)
infixl 4 `Snoc`
data Stream a = Cons a (Stream a)
  deriving Functor
infixr 4 `Cons`

-- We will keep INFINITE lists in our structure,
-- to get nice comonadic structure,
-- so we will have to later cut out the finite section that we
-- know is all that possibly lives
data CA a = CA (Bwd a) a (Stream a)
  deriving Functor

left :: CA a -> CA a
left (CA (Snoc ls l) a rs) = CA ls l (a `Cons` rs)

right :: CA a -> CA a
right (CA ls a (r`Cons`rs)) = CA (ls`Snoc`a) r rs

iterateBwd :: (a -> a) -> a -> Bwd a
iterateBwd f a = fmap f (iterateBwd f a) `Snoc` a

iterateStr :: (a -> a) -> a -> Stream a
iterateStr f a = a `Cons` fmap f (iterateStr f a)

tailBwd :: Bwd a -> Bwd a
tailBwd (xs `Snoc` _) = xs

tailStr :: Stream a -> Stream a
tailStr (_ `Cons` xs) = xs

repeatBwd :: a -> Bwd a
repeatBwd x = repeatBwd x `Snoc` x

repeatStr :: a -> Stream a
repeatStr x = x `Cons` repeatStr x

toStr :: [a] -> a -> Stream a
toStr xs x = foldr Cons (repeatStr x) xs

takeBwd :: Int -> Bwd a -> BwdL a
takeBwd n (xs`Snoc`x) | n<0 = Nil
                      | otherwise = takeBwd (n-1) xs `SnocL` x
takeStr :: Int -> Stream a -> [a]
takeStr n (x`Cons`xs) | n<0 = []
                      | otherwise = x : takeStr (n-1) xs

nbd :: Int -> CA a -> (BwdL a,a,[a])
nbd n (CA ls a rs) = (takeBwd n ls,a,takeStr n rs)

(<>>) :: BwdL a -> [a] -> [a]
Nil <>> ys = ys
SnocL xs x <>> ys = xs <>> (x : ys)


instance Comonad CA where
  extract (CA _ a _) = a
  duplicate ca = CA (tailBwd $ iterateBwd left ca)
                    ca
                    (tailStr $ iterateStr right ca)

-- we are given the initial state, and the local evolution
day12a_parser :: Parser (CA Bool, Int, CA Bool -> Bool)
day12a_parser = (\st rs -> (state st, length st, beh rs))
                 <$ string "initial state: " <*> many cell <* eol
                 <* eol
                 <*> many ((,) <$> many cell <* string " => " <*> cell <* eol)
  where cell = False <$ char '.'
           <|> True <$ char '#'
        -- given cells 0,1,2,...
        state [] = error "no initial pots given"
        state (x:xs) = CA (repeatBwd False) x (toStr xs False)
        -- hard code assumption that empty surroundings => empty cell
        beh rules | not $ (replicate 5 False, False) `elem` rules
          = error "huh?"
                  | otherwise = \cs -> snd $ head $ filter (flip match cs . fst) rules
        -- hard code rules are cell + 2 on each side
        match [b'2,b'1,b,b1,b2] (CA (_`Snoc`l2`Snoc`l1)
                                    c
                                    (r1`Cons`r2`Cons`rs))
          =  b'2 == l2 && b'1 == l1
          && c == b
          && b1 == r1 && b2 == r2

run :: (CA Bool, CA Bool -> Bool) -> [CA Bool]
run (st,beh) = iterate step st
  where step st = beh <$> duplicate st

showCA :: Int -> CA Bool -> String
showCA n ca = sh $ nbd n ca
  where sh (ls',c,rs) = let ls = ls' <>> []
                        in map sh' ls ++ sh' c : map sh' rs
        sh' True = '#'
        sh' False = '.'

alives :: (BwdL Bool,Bool,[Bool]) -> [Int]
alives (ls,c,rs) = let al = map snd $ filter fst $ zip (reverse $ ls <>> []) [-1,-2..]
                       ac = if c then [0] else []
                       ar = map snd $ filter fst $ zip rs [1..]
                   in al ++ ac ++ ar


day12a_solve :: (CA Bool, Int, CA Bool -> Bool) -> Either Void Int
day12a_solve (st, initialLen, beh) = pure $ sum $ alives $ nbd (iters * modCont + initialLen) $ run (st, beh) !! iters
  where iters = 20 -- given in problem text
        modCont = 2 -- speed of light is 2 cells per iter
        step st = beh <$> duplicate st
        -- focussed at zero

day12a_main :: IO ()
day12a_main = generic_main "../data/12a" day12a_parser day12a_solve show


-- 12b: now for 50 billion iterations
-- by inspection, it looks like it stabilizes as "everything move one cell right" after ~100 iterations

eqCA :: Eq a => Int -> CA a -> CA a -> Bool
eqCA n xs ys = nbd n xs == nbd n ys

day12b_solve :: (CA Bool, Int, CA Bool -> Bool) -> Either Void Integer
day12b_solve (st, initialLen, beh) =
  let modCont = 2
      n = 100
      [a,b] = take 2 $ drop n $ run (st, beh)
      nbdSize = modCont*n + initialLen
      end :: Integer
      end = 50000000000
  -- if agree on all cell may have touched,
  --then have settled down to just translating
  -- NB: left means move focus left!
  in if eqCA nbdSize (left a) b
     then let as = alives $ nbd nbdSize a
              finals = map (((end-fromIntegral n)+).fromIntegral) as
          in pure $ sum finals
     else error "Not just translating..."

day12b_main :: IO ()
day12b_main = generic_main "../data/12a" day12a_parser day12b_solve show

