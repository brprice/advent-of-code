{-# LANGUAGE NamedFieldPuns #-}

module Intcode where

import Control.Monad.State
import Data.Functor.Identity (Identity(Identity))
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

-- lookup with default 0
(!) :: M.Map Integer Integer -> Integer -> Integer
(!) = flip (M.findWithDefault 0)

getMem :: State IC Integer
getMem = do IC{ip,relBase,mem} <- get
            put (IC{ip=ip+1,relBase,mem})
            pure $ mem ! ip

getMemMode :: Mode -> State IC Integer
getMemMode Imm = getMem
getMemMode Pos = do p <- getMem
                    gets ((! p) . mem)
getMemMode Rel = do p <- getMem
                    IC {relBase,mem} <- get
                    pure $ mem ! (p+relBase)

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
run inputStream = StateT $ Identity . flip go inputStream
  where go :: IC -> [Integer] -> ([Integer],IC)
        go ic input = case runToIO ic of
                        ICIn f -> case input of
                                    [] -> error "Ran out of input!"
                                    (i:is) -> go (f i) is
                        ICOut o ic' -> let (os,ic'') = go ic' input
                                       in (o:os,ic'')
                        ICHalt ic' -> ([],ic')

-- For day 11, we can't get away with viewing running intcode as a function [Integer] -> [Integer].
data ICIO = ICIn (Integer -> IC) | ICOut Integer IC | ICHalt IC

runToIO :: IC -> ICIO
runToIO ic = case run1 ic of
               Cont1 ic' -> runToIO ic'
               In1 f -> ICIn f
               Out1 o ic' -> ICOut o ic'
               Halt1 ic' -> ICHalt ic'


-- For day 23, we will run many machines in lock-step, so we need run1
-- Massaging the above into a more useful form is annoyingly awkward,
-- but it works well enough to get the day done.
data ICRun1 = Cont1 IC | In1 (Integer -> IC) | Out1 Integer IC | Halt1 IC
run1 :: IC -> ICRun1
run1 ic = let (op,ic') = runState getOp ic
          in case op of
               Add am bm cm -> Cont1 $ execState (run_op2 (+) am bm cm) ic'
               Mul am bm cm -> Cont1 $ execState (run_op2 (*) am bm cm) ic'
               In m -> In1 $ \i -> execState (run_in i m) ic'
               Out m -> uncurry Out1 $ runState (run_out m) ic'
               Jit am bm -> Cont1 $ execState (run_ji (/=0) am bm) ic'
               Jif am bm -> Cont1 $ execState (run_ji (==0) am bm) ic'
               Lt am bm cm -> Cont1 $ execState (run_op2 lt am bm cm) ic'
               Eq am bm cm -> Cont1 $ execState (run_op2 eq am bm cm) ic'
               RB m -> Cont1 $ execState (run_rb m) ic'
               Halt -> Halt1 ic
  where lt x y = c_bool $ x < y
        eq x y = c_bool $ x == y
        c_bool True = 1
        c_bool False = 0
