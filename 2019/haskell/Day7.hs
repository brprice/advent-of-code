{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

module Main where

import Control.Monad.State
import qualified Data.IntMap as IM
import Data.List (permutations)
import Data.Monoid(Dual(Dual,getDual),Endo(Endo,appEndo))

data IC = IC {ip :: Int, mem :: IM.IntMap Int} deriving Show
data Mode = Pos | Imm deriving Enum
data Op = Add Mode Mode
        | Mul Mode Mode
        | In
        | Out Mode
        | Jit Mode Mode
        | Jif Mode Mode
        | Lt Mode Mode
        | Eq Mode Mode
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

setIP :: Int -> State IC ()
setIP ip = modify (\ic -> ic{ip})

getOp :: State IC Op
getOp = do opcode <- getMem
           let (ams,op) = opcode `quotRem` 100
           let argModes = am ams
           pure $ case op of
                    1 -> Add (argModes !! 0) (argModes !! 1)
                    2 -> Mul (argModes !! 0) (argModes !! 1)
                    3 -> In
                    4 -> Out (argModes !! 0)
                    5 -> Jit (argModes !! 0) (argModes !! 1)
                    6 -> Jif (argModes !! 0) (argModes !! 1)
                    7 -> Lt (argModes !! 0) (argModes !! 1)
                    8 -> Eq (argModes !! 0) (argModes !! 1)
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

run_ji :: (Int -> Bool) -> Mode -> Mode -> State IC ()
run_ji cond xm tm = do x <- getMemMode xm
                       tgt <- getMemMode tm
                       when (cond x) $ setIP tgt

run :: [Int] -> State IC [Int]
run input = do op <- getOp
               case op of
                 Add am bm -> run_op2 (+) am bm >> run input
                 Mul am bm -> run_op2 (*) am bm >> run input
                 In -> case input of
                   [] -> error "Ran out of input!"
                   (i:is) -> run_in i >> run is
                 Out m -> (:) <$> run_out m <*> run input
                 Jit am bm -> run_ji (/=0) am bm >> run input
                 Jif am bm -> run_ji (==0) am bm >> run input
                 Lt am bm -> run_op2 lt am bm >> run input
                 Eq am bm -> run_op2 eq am bm >> run input
                 Halt -> pure []
  where lt x y = c_bool $ x < y
        eq x y = c_bool $ x == y
        c_bool True = 1
        c_bool False = 0

rotateR :: [a] -> [a]
rotateR xs = last xs : init xs

-- Like zipWith, but assumes the first input list is the shorter of the two
-- and thus the output is the same length as the first input list
-- We require this extra laziness, otherwise part b <<loop>>s, because of the line
-- oss = zipWith phased ps $ rotateR oss
zipWithL :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithL _ [] _ = []
zipWithL f (x:xs) ~(y:ys) = f x y : zipWithL f xs ys

main :: IO ()
main = do cts <- readFile "../data/day7"
          let ops = IM.fromList $ zip [0..] $ map read $ words $ map (\c -> if c==',' then ' ' else c) cts :: IM.IntMap Int
          let phased p is = evalState (run $ p:is) (IC {ip=0,mem=ops})

          let phased1 p i = head $ phased p [i]
          let chained ps = appEndo $ getDual $ foldMap (Dual . Endo . phased1) ps
          putStr "part a: maximum thrusters "
          print $ maximum $ map (flip chained 0) $ permutations [0..4]

          let looped ps i = let oss :: [[Int]]
                                oss = zipWithL phased ps $ zipWith (++) ([i] : repeat []) $ rotateR oss
                            in last $ last oss
          putStr "part b: maximum loop-amplified thrusters "
          print $ maximum $ map (flip looped 0) $ permutations [5..9]
