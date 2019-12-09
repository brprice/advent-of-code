{-# LANGUAGE NamedFieldPuns #-}

module Intcode where

import Control.Monad.State
import qualified Data.Map as M

data IC = IC {ip :: Integer, relBase :: Integer, mem :: M.Map Integer Integer} deriving Show
data Mode = Pos | Imm | Rel deriving Enum
data Op = Add Mode Mode Mode
        | Mul Mode Mode Mode
        | In Mode
        | Out Mode
        | Jit Mode Mode
        | Jif Mode Mode
        | Lt Mode Mode Mode
        | Eq Mode Mode Mode
        | RB Mode
        | Halt

readIntcode :: FilePath -> IO IC
readIntcode f = do cts <- readFile f
                   pure $ IC 0 0 $ M.fromList $ zip [0..] $ map read $ words $ map (\c -> if c==',' then ' ' else c) cts


getMem :: State IC Integer
getMem = do IC{ip,relBase,mem} <- get
            put (IC{ip=ip+1,relBase,mem})
            pure $ mem M.! ip

getMemMode :: Mode -> State IC Integer
getMemMode Imm = getMem
getMemMode Pos = do p <- getMem
                    gets ((M.! p) . mem)
getMemMode Rel = do p <- getMem
                    IC {relBase,mem} <- get
                    pure $ mem M.! (p+relBase)

putMemMode :: Integer -> Mode -> Integer -> State IC ()
putMemMode _ Imm _ = error "Cannot write with Imm mode"
putMemMode dst Pos v = modify (\IC{ip,relBase,mem} -> IC{ip,relBase,mem=M.insert dst v mem})
putMemMode off Rel v = modify (\IC{ip,relBase,mem} -> IC{ip,relBase,mem=M.insert (relBase + off) v mem})

setIP :: Integer -> State IC ()
setIP ip = modify (\ic -> ic{ip})

getOp :: State IC Op
getOp = do opcode <- getMem
           let (ams,op) = opcode `quotRem` 100
           let argModes = am ams
           pure $ case op of
                    1 -> Add (argModes !! 0) (argModes !! 1) (notImm $ argModes !! 2)
                    2 -> Mul (argModes !! 0) (argModes !! 1) (notImm $ argModes !! 2)
                    3 -> In (notImm $ argModes !! 0)
                    4 -> Out (argModes !! 0)
                    5 -> Jit (argModes !! 0) (argModes !! 1)
                    6 -> Jif (argModes !! 0) (argModes !! 1)
                    7 -> Lt (argModes !! 0) (argModes !! 1) (notImm $ argModes !! 2)
                    8 -> Eq (argModes !! 0) (argModes !! 1) (notImm $ argModes !! 2)
                    9 -> RB (argModes !! 0)
                    99 -> Halt
                    _ -> error $ "Unrecognised opcode: " ++ show op
  where am :: Integer -> [Mode]
        am n = let (n',m') = n`quotRem`10
               in toEnum (fromInteger m') : am n'
        notImm :: Mode -> Mode
        notImm Imm = error "Unexpected Immediate mode"
        notImm m = m

run_op2 :: (Integer -> Integer -> Integer) -> Mode -> Mode -> Mode -> State IC ()
run_op2 f am bm cm = do a <- getMemMode am
                        b <- getMemMode bm
                        dst <- getMem
                        putMemMode dst cm $ f a b

run_in :: Integer -> Mode -> State IC ()
run_in input m = do dst <- getMem
                    putMemMode dst m input

run_out :: Mode -> State IC Integer
run_out = getMemMode

run_ji :: (Integer -> Bool) -> Mode -> Mode -> State IC ()
run_ji cond xm tm = do x <- getMemMode xm
                       tgt <- getMemMode tm
                       when (cond x) $ setIP tgt

run_rb :: Mode -> State IC ()
run_rb m = do off <- getMemMode m
              ic <- get
              put $ ic{relBase = relBase ic + off}

run :: [Integer] -> State IC [Integer]
run input = do op <- getOp
               case op of
                 Add am bm cm -> run_op2 (+) am bm cm >> run input
                 Mul am bm cm -> run_op2 (*) am bm cm >> run input
                 In m -> case input of
                   [] -> error "Ran out of input!"
                   (i:is) -> run_in i m >> run is
                 Out m -> (:) <$> run_out m <*> run input
                 Jit am bm -> run_ji (/=0) am bm >> run input
                 Jif am bm -> run_ji (==0) am bm >> run input
                 Lt am bm cm -> run_op2 lt am bm cm >> run input
                 Eq am bm cm -> run_op2 eq am bm cm >> run input
                 RB m -> run_rb m >> run input
                 Halt -> pure []
  where lt x y = c_bool $ x < y
        eq x y = c_bool $ x == y
        c_bool True = 1
        c_bool False = 0
