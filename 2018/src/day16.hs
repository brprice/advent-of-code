-- Day 16: simulate a simple cpu
-- 4 registers: 0..3, seem to contain small integers (not specified in problem)
-- 16 instructions

import Data.Bits ((.|.),(.&.))
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Void

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Util

data Reg = R0 | R1 | R2 | R3
data State = State Int Int Int Int deriving (Show, Eq)

get :: State -> Reg -> Int
get (State r0 _ _ _) R0 = r0
get (State _ r1 _ _) R1 = r1
get (State _ _ r2 _) R2 = r2
get (State _ _ _ r3) R3 = r3

set :: State -> Reg -> Int -> State
set (State _ r1 r2 r3) R0 n = State n r1 r2 r3
set (State r0 _ r2 r3) R1 n = State r0 n r2 r3
set (State r0 r1 _ r3) R2 n = State r0 r1 n r3
set (State r0 r1 r2 _) R3 n = State r0 r1 r2 n

setBit :: State -> Reg -> Bool -> State
setBit s r True = set s r 1
setBit s r False = set s r 0

data OpCode = CAddR | CAddI | CMulR | CMulI
            | CBAnR | CBAnI | CBOrR | CBOrI
            | CSetR | CSetI | CGtIR | CGtRI
            | CGtRR | CEqIR | CEqRI | CEqRR deriving (Eq, Ord, Enum, Show)

allOpCodes :: [OpCode]
allOpCodes = [CAddR .. CEqRR]

data Op = AddR Reg Reg Reg
        | AddI Reg Int Reg
        | MulR Reg Reg Reg
        | MulI Reg Int Reg
        | BAnR Reg Reg Reg
        | BAnI Reg Int Reg
        | BOrR Reg Reg Reg
        | BOrI Reg Int Reg
        | SetR Reg Reg -- NB: middle operand ignored
        | SetI Int Reg -- NB: middle operand ignored
        | GtIR Int Reg Reg
        | GtRI Reg Int Reg
        | GtRR Reg Reg Reg
        | EqIR Int Reg Reg
        | EqRI Reg Int Reg
        | EqRR Reg Reg Reg

code :: Op -> OpCode
code (AddR _ _ _) = CAddR
code (AddI _ _ _) = CAddI
code (MulR _ _ _) = CMulR
code (MulI _ _ _) = CMulI
code (BAnR _ _ _) = CBAnR
code (BAnI _ _ _) = CBAnI
code (BOrR _ _ _) = CBOrR
code (BOrI _ _ _) = CBOrI
code (SetR _ _) = CSetR
code (SetI _ _) = CSetI
code (GtIR _ _ _) = CGtIR
code (GtRI _ _ _) = CGtRI
code (GtRR _ _ _) = CGtRR
code (EqIR _ _ _) = CEqIR
code (EqRI _ _ _) = CEqRI
code (EqRR _ _ _) = CEqRR


doArithR :: (Int -> Int -> Int) -> Reg -> Reg -> Reg -> State -> State
doArithR f ra rb rc s =  set s rc $ f (get s ra) (get s rb)

doArithI :: (Int -> Int -> Int) -> Reg -> Int -> Reg -> State -> State
doArithI f ra i rc s =  set s rc $ f (get s ra) i


doOp :: Op -> State -> State
doOp (AddR ra rb rc) s = doArithR (+) ra rb rc s
doOp (AddI ra i rc) s = doArithI (+) ra i rc s
doOp (MulR ra rb rc) s = doArithR (*) ra rb rc s
doOp (MulI ra i rc) s = doArithI (*) ra i rc s
doOp (BAnR ra rb rc) s = doArithR (.&.) ra rb rc s
doOp (BAnI ra i rc) s = doArithI (.&.) ra i rc s
doOp (BOrR ra rb rc) s = doArithR (.|.) ra rb rc s
doOp (BOrI ra i rc) s = doArithI (.|.) ra i rc s
doOp (SetR ra rc) s = set s rc $ get s ra
doOp (SetI i rc) s = set s rc i
doOp (GtIR i rb rc) s = setBit s rc $ i > get s rb
doOp (GtRI ra i rc) s = setBit s rc $ get s ra > i
doOp (GtRR ra rb rc) s = setBit s rc $ get s ra > get s rb
doOp (EqIR i rb rc) s = setBit s rc $ i == get s rb
doOp (EqRI ra i rc) s = setBit s rc $ get s ra == i
doOp (EqRR ra rb rc) s = setBit s rc $ get s ra == get s rb


-- 16a: Given samples of the form
--
-- Before: [3, 2, 1, 1]
-- 9 2 1 2
-- After:  [3, 2, 2, 1]
--
-- Where we are given the numeric value 9 of the opcode.
-- How many opcodes could 9 possibly be (here, 3: MulR, AddI, SetI)?
-- In particular, how many samples could be 3 or more different opcodes?

data Sample = Sample {before :: State
                     ,instr :: (Int,Int,Int,Int)
                     ,after :: State}
type Program = [(Int,Int,Int,Int)]

-- We'll parse both parts together
day16_parser :: Parser ([Sample],Program)
day16_parser = (,) <$> many (Sample <$> state "Before: "
                                    <*> inst
                                    <*> state "After:  "
                                    <* eol)
                   <* eol <* eol
                   <*> many inst
  where state :: String -> Parser State
        state s = State <$ string s <* string "["
                        <*> decimal <* string ", "
                        <*> decimal <* string ", "
                        <*> decimal <* string ", "
                        <*> decimal <* string "]"
                        <* eol
        inst = (,,,) <$> decimal <* string " "
                     <*> decimal <* string " "
                     <*> decimal <* string " "
                     <*> decimal
                     <* eol

testSample :: (State,Op,State) -> Bool
testSample (bef,op,aft) = doOp op bef == aft

toReg :: Int -> Maybe Reg
toReg 0 = Just R0
toReg 1 = Just R1
toReg 2 = Just R2
toReg 3 = Just R3
toReg _ = Nothing

mkRRR :: (Reg -> Reg -> Reg -> Op) -> Int -> Int -> Int -> Maybe Op
mkRRR op a b c = do ra <- toReg a
                    rb <- toReg b
                    rc <- toReg c
                    pure $ op ra rb rc

mkRIR :: (Reg -> Int -> Reg -> Op) -> Int -> Int -> Int -> Maybe Op
mkRIR op a b c = do ra <- toReg a
                    rc <- toReg c
                    pure $ op ra b rc

mkR_R :: (Reg -> Reg -> Op) -> Int -> Int -> Int -> Maybe Op
mkR_R op a _ c = do ra <- toReg a
                    rc <- toReg c
                    pure $ op ra rc

mkI_R :: (Int -> Reg -> Op) -> Int -> Int -> Int -> Maybe Op
mkI_R op a _ c = do rc <- toReg c
                    pure $ op a rc

mkIRR :: (Int -> Reg -> Reg -> Op) -> Int -> Int -> Int -> Maybe Op
mkIRR op a b c = do rb <- toReg b
                    rc <- toReg c
                    pure $ op a rb rc

-- What operation might it be?
mkOp :: OpCode -> Int -> Int -> Int -> Maybe Op
mkOp CAddR = mkRRR AddR
mkOp CAddI = mkRIR AddI
mkOp CMulR = mkRRR MulR
mkOp CMulI = mkRIR MulI
mkOp CBAnR = mkRRR BAnR
mkOp CBAnI = mkRIR BAnI
mkOp CBOrR = mkRRR BOrR
mkOp CBOrI = mkRIR BOrI
mkOp CSetR = mkR_R SetR
mkOp CSetI = mkI_R SetI
mkOp CGtIR = mkIRR GtIR
mkOp CGtRI = mkRIR GtRI
mkOp CGtRR = mkRRR GtRR
mkOp CEqIR = mkIRR EqIR
mkOp CEqRI = mkRIR EqRI
mkOp CEqRR = mkRRR EqRR

reify :: Sample -> [(State,Op,State)]
reify (Sample bef (_,a,b,c) aft)
  = map (\op -> (bef,op,aft))
  $ catMaybes $ map (\f -> f a b c) reify'
  where reify' = map mkOp allOpCodes

day16a_solve :: [Sample] -> Either Void Int
day16a_solve = pure . length . filter ambig3
  where ambig3 = (>=3) . length . filter testSample . reify

day16a_main :: IO ()
day16a_main = generic_main "../data/16a" (fst <$> day16_parser) day16a_solve show


-- 16b: work out what number each opcode has, then execute the program
-- what is the final value in R0?
-- (initial values not specified, so guess don't matter, and I'll set to 0)
-- (Turns out, only the initial value of R3 matters, and it _DOES_ matter!)
-- (but initial value of 0 is what gives the accepted answer)

get_ops :: [Sample] -> Either String (M.Map Int OpCode)
get_ops ss = M.traverseWithKey (\k v -> if S.size v == 1
                                        then Right $ S.findMin v
                                        else Left $ "Cannot work out instruction "
                                               ++ show k
                                               ++ " the possibilities are "
                                               ++ show v)
           $ uniqueMapping
           $ foldr (M.unionWith S.intersection) allPoss
           $ map poss ss
  where allPoss = M.fromList [(i,S.fromList allOpCodes)
                             | i <- [0..15]]
        poss s = M.singleton ((\(op,_,_,_) -> op) $ instr s)
               $ S.fromList
               $ map (\(_,op,_) -> code op)
               $ filter testSample
               $ reify s
        uniqueMapping codes = let (uniq,rest) = M.partition ((==1) . S.size)
                                                            codes
                                  uniqs = S.unions $ M.elems uniq
                              in uniqueMapping' uniqs uniq rest
        uniqueMapping' uniqs uniq rest
          = let rest' = M.map (`S.difference`uniqs) rest
                (uniq',rest'') = M.partition ((==1) . S.size) rest'
            in if M.null uniq'
               then M.union uniq rest
               else uniqueMapping' (S.union uniqs $ S.unions $ M.elems uniq')
                                   (M.union uniq uniq')
                                   rest''

mkOp' :: M.Map Int OpCode -> (Int,Int,Int,Int) -> Either String Op
mkOp' ops (i,a,b,c) = maybe (Left $ "Can't make a " ++ show op
                                 ++ " out of " ++ show (i,a,b,c))
                            Right
                    $ mkOp op a b c
  where op = ops M.! i

runProg :: [Op] -> State -> State
runProg os s = foldl (flip doOp) s os

day16b_solve :: ([Sample],Program) -> Either String Int
day16b_solve (ss,p) = do ops <- get_ops ss
                         prog <- traverse (mkOp' ops) p
                         pure $ get (runProg prog initState) R0
  where initState = State 0 0 0 0

day16b_main :: IO ()
day16b_main = generic_main "../data/16a" day16_parser day16b_solve show
