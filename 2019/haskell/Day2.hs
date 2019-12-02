{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

main :: IO ()
main = do cts <- readFile "../data/day2"
          let ops = V.fromList $ map read $ words $ map (\c -> if c==',' then ' ' else c) cts :: V.Vector Int
          let result = V.modify (\v -> MV.write v 1 12 >> MV.write v 2 2 >> run v) ops
          putStrLn $ "part a: running in the 1202 configuration gives output: " ++ show (result V.! 0)

run :: forall s . V.MVector s Int -> ST s ()
run ops = go 0
  where go :: Int -> ST s ()
        go ip = do op <- MV.read ops ip
                   d <- MV.read ops (ip+3)
                   a <- MV.read ops (ip+1)
                   va <- MV.read ops a
                   b <- MV.read ops (ip+2)
                   vb <- MV.read ops b
                   case op of
                     1 -> MV.write ops d (va + vb) >> go (ip + 4)
                     2 -> MV.write ops d (va * vb) >> go (ip + 4)
                     99 -> pure ()
                     o -> error $ "Invalid opcode: " ++ show o
