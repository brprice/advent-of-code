{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

main :: IO ()
main = do cts <- readFile "../data/day2"
          let ops = V.fromList $ map read $ words $ map (\c -> if c==',' then ' ' else c) cts :: V.Vector Int
          let result = V.modify (run 12 2) ops
          putStrLn $ "part a: running in the 1202 configuration gives output: " ++ show (result V.! 0)

          let target = 19690720
          putStrLn $ "part b: now searching for noun/verb pair giving output of " ++ show target
          let nvs = [(n,v) | n<-[0..99],v<-[0..99], V.modify (run n v) ops V.! 0 == target]
          let (n,v) = head nvs
          putStrLn $ "part b: found noun = " ++ show n ++ ", verb = " ++ show v
          putStrLn $ "part b: thus answer is " ++ show (100*n+v)

run :: forall s . Int -> Int -> V.MVector s Int -> ST s ()
run n v ops = MV.write ops 1 n >> MV.write ops 2 v >> go 0
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
