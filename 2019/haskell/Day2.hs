{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad.State
import qualified Data.IntMap as IM

import Intcode

main :: IO ()
main = do mach <- readIntcode "../data/day2"
          let result = execState (runNV 12 2) mach
          putStrLn $ "part a: running in the 1202 configuration gives output: " ++ show (mem result IM.! 0)

          let target = 19690720
          putStrLn $ "part b: now searching for noun/verb pair giving output of " ++ show target
          let nvs = [(n,v) | n<-[0..99],v<-[0..99], mem (execState (runNV n v) mach) IM.! 0 == target]
          let (n,v) = head nvs
          putStrLn $ "part b: found noun = " ++ show n ++ ", verb = " ++ show v
          putStrLn $ "part b: thus answer is " ++ show (100*n+v)

runNV :: Int -> Int -> State IC [Int]
runNV n v = do IC{ip,mem} <- get
               put $ IC{ip,mem=IM.insert 1 n $ IM.insert 2 v mem}
               run []
