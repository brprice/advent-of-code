{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Char (ord)
import Data.Functor ((<&>))
import Data.List (foldl')

getData :: IO String
getData = readFile "../data/day3"

spanJust :: (a -> Maybe b) -> [a] -> ([b], [a])
spanJust f = \case
  [] -> ([], [])
  a : as
    | Just b <- f a -> let (l, r) = spanJust f as in (b : l, r)
    | otherwise -> ([], a : as)

maybeDigit :: Char -> Maybe Integer
maybeDigit c =
  let d = ord c - ord '0'
   in if 0 <= d && d <= 9
        then pure $ fromIntegral d
        else Nothing

readInteger :: String -> Maybe (Integer, String)
readInteger =
  spanJust maybeDigit <&> \case
    ([], _) -> Nothing
    (ds, s) -> Just (fromDigits ds, s)

fromDigits :: [Integer] -> Integer
fromDigits = foldl' (\acc d -> 10 * acc + d) 0

-- Parsing would be easy with regex, but it is simple enough to write myself
data Token
  = Mul
  | ParenL
  | ParenR
  | Comma
  | I Integer
  | Garbage Char
  deriving (Show)

tokenize :: String -> [Token]
tokenize = \case
  [] -> []
  'm' : 'u' : 'l' : ss -> Mul : tokenize ss
  '(' : ss -> ParenL : tokenize ss
  ')' : ss -> ParenR : tokenize ss
  ',' : ss -> Comma : tokenize ss
  s : ss
    | Just (n, ss') <- readInteger (s : ss) -> I n : tokenize ss'
    | otherwise -> Garbage s : tokenize ss

data Instr = Mult Integer Integer
  deriving (Show)

parse :: [Token] -> [Instr]
parse = \case
  [] -> []
  Mul : ParenL : I a : Comma : I b : ParenR : ts -> Mult a b : parse ts
  _ : ts -> parse ts

part1 :: [Instr] -> Integer
part1 = sum . map (\(Mult x y) -> x * y)

main :: IO ()
main = do
  xs <- parse . tokenize <$> getData
  putStrLn "Part 1"
  print $ part1 xs
