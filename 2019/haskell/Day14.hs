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

main :: IO ()
main = do dat <- readFile "../data/day14"
          let Right recipes' = runParser parser "" dat
          let recipes = M.fromList $ map (\(inp,(outn,outc)) -> (outc,(outn,inp))) recipes'
          putStr "part a: ore to make 1 fuel: "
          print $ oreToBuild recipes 1 "FUEL"
