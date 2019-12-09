{-# LANGUAGE NamedFieldPuns #-}

module Intcode where

import Control.Monad.State
import qualified Data.Map as M

data IC = IC {ip :: Integer, mem :: M.Map Integer Integer} deriving Show
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

readIntcode :: FilePath -> IO IC
readIntcode f = do cts <- readFile f
                   pure $ IC 0 $ M.fromList $ zip [0..] $ map read $ words $ map (\c -> if c==',' then ' ' else c) cts


getMem :: State IC Integer
getMem = do IC{ip,mem} <- get
            put (IC{ip=ip+1,mem})
            pure $ mem M.! ip

getMemMode :: Mode -> State IC Integer
getMemMode Imm = getMem
getMemMode Pos = do p <- getMem
                    gets ((M.! p) . mem)

putMem :: Integer -> Integer -> State IC ()
putMem dst v = modify (\IC{ip,mem} -> IC{ip,mem=M.insert dst v mem})

setIP :: Integer -> State IC ()
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
  where am :: Integer -> [Mode]
        am n = let (n',m') = n`quotRem`10
               in toEnum (fromInteger m') : am n' -- assume no overflow here

run_op2 :: (Integer -> Integer -> Integer) -> Mode -> Mode -> State IC ()
run_op2 f am bm = do a <- getMemMode am
                     b <- getMemMode bm
                     dst <- getMem
                     putMem dst $ f a b

run_in :: Integer -> State IC ()
run_in input = do dst <- getMem
                  putMem dst input

run_out :: Mode -> State IC Integer
run_out = getMemMode

run_ji :: (Integer -> Bool) -> Mode -> Mode -> State IC ()
run_ji cond xm tm = do x <- getMemMode xm
                       tgt <- getMemMode tm
                       when (cond x) $ setIP tgt

run :: [Integer] -> State IC [Integer]
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
