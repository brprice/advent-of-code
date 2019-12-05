{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

module Main where

import Control.Monad.State
import qualified Data.IntMap as IM

main :: IO ()
main = do cts <- readFile "../data/day5"
          let ops = IM.fromList $ zip [0..] $ map read $ words $ map (\c -> if c==',' then ' ' else c) cts :: IM.IntMap Int
          let resulta = evalState (run [1]) $ IC {ip = 0, mem=ops}
          let (_,rest) = span (==0) resulta
          case rest of
            [a] -> putStrLn $ "part a: " ++ show a
            nzs -> putStrLn $ "part a: failure: non-zero output was " ++ show nzs

data IC = IC {ip :: Int, mem :: IM.IntMap Int} deriving Show
data Mode = Pos | Imm deriving Enum
data Op = Add Mode Mode
        | Mul Mode Mode
        | In
        | Out Mode
        | Halt

getMem :: State IC Int
getMem = do IC{ip,mem} <- get
            put (IC{ip=ip+1,mem})
            pure $ mem IM.! ip

getMemMode :: Mode -> State IC Int
getMemMode Imm = getMem
getMemMode Pos = do p <- getMem
                    gets ((IM.! p) . mem)

putMem :: Int -> Int -> State IC ()
putMem dst v = modify (\IC{ip,mem} -> IC{ip,mem=IM.insert dst v mem})

getOp :: State IC Op
getOp = do opcode <- getMem
           let (ams,op) = opcode `quotRem` 100
           let argModes = am ams
           pure $ case op of
                    1 -> Add (argModes !! 0) (argModes !! 1)
                    2 -> Mul (argModes !! 0) (argModes !! 1)
                    3 -> In
                    4 -> Out (argModes !! 0)
                    99 -> Halt
                    _ -> error $ "Unrecognised opcode: " ++ show op
  where am :: Int -> [Mode]
        am n = let (n',m') = n`quotRem`10
               in toEnum m' : am n'

run_op2 :: (Int -> Int -> Int) -> Mode -> Mode -> State IC ()
run_op2 f am bm = do a <- getMemMode am
                     b <- getMemMode bm
                     dst <- getMem
                     putMem dst $ f a b

run_in :: Int -> State IC ()
run_in input = do dst <- getMem
                  putMem dst input

run_out :: Mode -> State IC Int
run_out = getMemMode

run :: [Int] -> State IC [Int]
run input = do op <- getOp
               case op of
                 Add am bm -> run_op2 (+) am bm >> run input
                 Mul am bm -> run_op2 (*) am bm >> run input
                 In -> case input of
                   [] -> error "Ran out of input!"
                   (i:is) -> run_in i >> run is
                 Out m -> (:) <$> run_out m <*> run input
                 Halt -> pure []
