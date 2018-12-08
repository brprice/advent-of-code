import Control.Monad

import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Util

data Tree a = N [Tree a] [a] deriving Show

cata :: ([b] -> [a] -> b) -> Tree a -> b
cata f (N cs ds) = f (map (cata f) cs) ds

-- 8a: given a tree in form #children #metadata children... meta...
-- find sum of all metadata
parseTree :: Parser (Tree Int)
parseTree = do nc <- decimal
               single ' '
               nd <- decimal
               children <- replicateM nc (single ' ' *> parseTree)
               meta <- replicateM nd (single ' ' *> decimal)
               pure $ N children meta

day8a_parser :: Parser (Tree Int)
day8a_parser = parseTree <* eol

day8a_solve :: Tree Int -> Either Void Int
day8a_solve = Right . cata (\rs ds -> sum rs + sum ds)

day8a_main :: IO ()
day8a_main = generic_main "../data/8a" day8a_parser day8a_solve show

-- 8b: calculate "value", defined as
-- If no children, sum metadata
-- If has children, sum [value (child i) | i<-metadata]
-- where children are 1-indexed, and non-existant indexes count as 0
day8b_solve :: Tree Int -> Either Void Int
day8b_solve = Right . cata v
  where v [] ds = sum ds
        v vs ds = sum [vs!d | d<-ds]
        []!_ = 0
        xs!0 = 0
        (x:xs)!n | n==1 = x
                 | n>1 = xs!(n-1)
                 | otherwise = 0
day8b_main :: IO ()
day8b_main = generic_main "../data/8a" day8a_parser day8b_solve show
