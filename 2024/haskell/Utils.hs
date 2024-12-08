module Utils
  ( Grid (..),
    parseGrid,
  )
where

import Data.Map qualified as M
import Data.Maybe (mapMaybe)

data Grid a = Grid
  { width, height :: Int,
    -- coordinates are (0,0) is top-left
    -- and (width-1,height-1) is bottom-right
    cts :: M.Map (Int, Int) a
  }
  deriving (Show)

parseGrid :: (Char -> Maybe a) -> String -> Grid a
parseGrid f s =
  let ls = lines s
      l' y = mapMaybe (traverse f) . zip (map (,y) [0 ..])
      ls' = concatMap (uncurry l') . zip [0 ..]
      cts = ls' ls
   in Grid
        { width = length (head ls),
          height = length ls,
          cts = M.fromList cts
        }
