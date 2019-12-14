module Main where

import Control.Lens
import qualified Data.Map as M
import Data.Void(Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (signed,decimal)

parser :: Parsec Void String [([(Integer,String)],(Integer,String))]
parser = many (parseLine <* eol) <* eof
  where parseLine = (,) <$> sepBy parseChem (string ", ") <* string " => " <*> parseChem
        parseChem = (,) <$> signed (pure ()) decimal <* space <*> many upperChar

type Recipes = M.Map String (Integer,[(Integer,String)])

oreToBuild :: Recipes -> Integer -> String -> Integer
oreToBuild recipes n c = go [(n,c)] M.empty
  where go [] _ = 0
        go ((oreN,"ORE"):want) leftover = oreN + go want leftover
        go ((wn,wc):want) leftover = let have = M.findWithDefault 0 wc leftover
                                     in if wn < have
                                        then go want $ leftover & ix wc -~ wn
                                        else let (rn,rin) = recipes ^?! ix wc
                                                 toMake = wn - have
                                                 (q,r) = toMake`quotRem`rn
                                                 times = q + if r > 0 then 1 else 0
                                                 inputs = rin & each . _1 %~ (*times)
                                             in go (inputs ++ want) $ leftover & at wc ?~ (rn*times-toMake)

-- Assuming the predicate is monotonic, find the first input making it true
expSearch :: (Integer -> Bool) -> Integer
expSearch p | p 0 = 0
            | p 1 = 1
            | otherwise = go 1
  where go falsy = let next = 2*falsy
                   in if p next
                      then binSearch p falsy next
                      else go next

-- Assuming the predicate is monotonic, find the first input making it true within a range
binSearch :: (Integer -> Bool) -> Integer -> Integer -> Integer
binSearch p lo hi | hi-lo == 1 = hi
                  | otherwise = let mid = (hi+lo)`div`2
                                in if p mid
                                   then binSearch p lo mid
                                   else binSearch p mid hi

main :: IO ()
main = do dat <- readFile "../data/day14"
          let Right recipes' = runParser parser "" dat
          let recipes = M.fromList $ map (\(inp,(outn,outc)) -> (outc,(outn,inp))) recipes'
          putStr "part a: ore to make 1 fuel: "
          print $ oreToBuild recipes 1 "FUEL"

          -- part b: how much fuel can we make with 1 trillion ore
          -- a simple exponential search is good enough here,
          -- no need for cleverness
          let maxOre = 10^12
          let minBad = expSearch (\n -> oreToBuild recipes n "FUEL" > maxOre)
          putStrLn $ "part b: with 1 trillion ore, we can make " ++ show (minBad - 1) ++ " fuel"
