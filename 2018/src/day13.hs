import Data.Bifunctor
import qualified Data.Map as M
import Data.Void

import Text.Megaparsec

import Util

-- 13a: Given a network of tracks and carts,
-- run it and find the first crash

-- RefS is '/', RefB is '\', act like corners (maybe mirrors if -\-, I guess)
-- Cross is a crossing '+'
-- Carts have state : Left ~> Straight ~> Right ~> Left ~> ...
-- of which direction they will go at the next crossing
data Track = Vert | Horiz | Cross | RefS | RefB
data NextTurn = TL | TS | TR deriving Show
data Dir = R | U | L | D deriving (Enum, Show)
type Cart = (Dir, NextTurn)

refS :: Dir -> Dir
refS = (!!) [U,R,D,L] . fromEnum

refB :: Dir -> Dir
refB = (!!) [D,L,U,R] . fromEnum

turn :: NextTurn -> Dir -> Dir
turn TL = (!!) [U,L,D,R] . fromEnum
turn TS = id
turn TR = (!!) [D,R,U,L] . fromEnum

nextTurn :: NextTurn -> NextTurn
nextTurn TL = TS
nextTurn TS = TR
nextTurn TR = TL

-- Carts move in turn, based on their current position
-- so we keep a list of carts in order of position


-- x,y
data V2 = V2 {_x, _y :: Int} deriving Eq
instance Show V2 where
  show (V2 x y) = show (x,y)

instance Ord V2 where
  compare (V2 x1 y1) (V2 x2 y2) = case compare y1 y2 of
    LT -> LT
    EQ -> compare x1 x2
    GT -> GT

move' :: V2 -> Dir -> V2
move' (V2 x y) R = V2 (x+1) y
move' (V2 x y) U = V2 x (y-1)
move' (V2 x y) L = V2 (x-1) y
move' (V2 x y) D = V2 x (y+1)

debug :: M.Map V2 Track -> [(V2,Cart)] -> String
debug ts cs = unlines $ [[go (V2 x y) | x<-[xmin..xmax]] | y<-[ymin..ymax]]
  where (xmin,xmax,ymin,ymax) = let ts' = map fst $ M.toList ts
                                in (minimum $ map _x ts'
                                   ,maximum $ map _x ts'
                                   ,minimum $ map _y ts'
                                   ,maximum $ map _y ts')
        cs' = M.fromList cs
        go p = case M.lookup p cs' of
                 Just c -> showC c
                 Nothing -> case M.lookup p ts of
                   Nothing -> ' '
                   Just t -> showT t
        showC (R,_) = '>'
        showC (U,_) = '^'
        showC (L,_) = '<'
        showC (D,_) = 'v'
        showT Horiz = '-'
        showT Vert = '|'
        showT Cross = '+'
        showT RefS = '/'
        showT RefB = '\\'

debug' :: M.Map V2 Track -> [(V2,Cart)] -> String
debug' ts cs = unlines $ go ts cs
  where go ts cs = case tick ts cs of
                     Left p -> [{-show cs, -}debug ts cs ,"\n", "CRASH: " ++ show p]
                     Right cs' -> --show cs : go ts cs'
                                  debug ts cs : "\n" : go ts cs'


-- Don't actually use Parsec, but do it like this to
-- keep in general framework
day13a_parser :: Parser (M.Map V2 Track, [(V2,Cart)])
day13a_parser = go <$> takeWhileP Nothing (const True)
  where go s = let (t,c) = go'
                         $ concatMap (\(y,l) -> zip (zip [0..] $ repeat y) l)
                         $ zip [0..]
                         $ lines s
               in (M.fromList $ map (first $ uncurry V2) t
                  , map (first $ uncurry V2) c)
        go' [] = ([],[])
        go' ((p,' '):ss) = go' ss
        go' ((p,'-'):ss) = first ((p,Horiz):) $ go' ss
        go' ((p,'|'):ss) = first ((p,Vert):) $ go' ss
        go' ((p,'/'):ss) = first ((p,RefS):) $ go' ss
        go' ((p,'\\'):ss) = first ((p,RefB):) $ go' ss
        go' ((p,'+'):ss) = first ((p,Cross):) $ go' ss
        go' ((p,'>'):ss) = bimap ((p,Horiz):) ((p,(R,TL)):) $ go' ss
        go' ((p,'^'):ss) = bimap ((p,Vert):) ((p,(U,TL)):) $ go' ss
        go' ((p,'<'):ss) = bimap ((p,Horiz):) ((p,(L,TL)):) $ go' ss
        go' ((p,'v'):ss) = bimap ((p,Vert):) ((p,(D,TL)):) $ go' ss


day13a_solve :: (M.Map V2 Track, [(V2,Cart)]) -> Either Void V2
day13a_solve (t,c) = pure $ go c
  where go c = case tick t c of
                 Left cp -> cp
                 Right c' -> go c'

tick :: M.Map V2 Track -> [(V2,Cart)] -> Either V2 [(V2,Cart)]
tick ts cs = tick' [] cs
  where tick' done [] = Right done
        tick' done (c:cs) = let c' = moveCart c
                            in case (ins done c',ins cs c') of
                                 -- check if crashes with not-yet-moved cart also!
                                 (Nothing, _) -> Left (fst c')
                                 (_, Nothing) -> Left (fst c')
                                 (Just done', Just _) -> tick' done' cs
        moveCart (p,(d,tn)) = let p' = move' p d
                              in case ts M.! p' of
                                   RefS -> (p',(refS d,tn))
                                   RefB -> (p',(refB d,tn))
                                   Cross -> (p',(turn tn d,nextTurn tn))
                                   _ -> (p',(d,tn))

        ins [] (p,c) = Just [(p,c)]
        ins ((p',c'):pcs) (p,c) = case compare p' p of
                                    LT -> ((p',c'):) <$> ins pcs (p,c)
                                    EQ -> Nothing -- Crash!
                                    GT -> Just $ (p,c):(p',c'):pcs


day13a_main :: IO ()
day13a_main = generic_main "../data/13a" day13a_parser day13a_solve show


-- 13b: similar, but when crash, remove the two carts
-- where is the last cart standing?

day13b_solve :: (M.Map V2 Track, [(V2,Cart)]) -> Either Void V2
day13b_solve (t,c) = pure $ go c
  where go c = case tickb t c of
                 [lastCart] -> fst lastCart
                 cs -> go cs

tickb :: M.Map V2 Track -> [(V2,Cart)] -> [(V2,Cart)]
tickb ts cs = tick' [] cs
  where tick' done [] = done
        tick' done (c:cs) = let c' = moveCart c
                            in case cartCrash cs c' of
                                 Nothing -> tick' (ins done c') cs -- no crash with cs
                                 Just cs' -> case cartCrash done c' of -- crash with cs
                                               Nothing -> tick' done cs' -- no crash with done
                                               Just done' -> tick' done' cs' --crash with done
        moveCart (p,(d,tn)) = let p' = move' p d
                              in case ts M.! p' of
                                   RefS -> (p',(refS d,tn))
                                   RefB -> (p',(refB d,tn))
                                   Cross -> (p',(turn tn d,nextTurn tn))
                                   _ -> (p',(d,tn))

        ins [] (p,c) = [(p,c)]
        ins ((p',c'):pcs) (p,c) = case compare p' p of
                                    LT -> (p',c'): ins pcs (p,c)
                                    EQ -> pcs -- Crash!
                                    GT -> (p,c):(p',c'):pcs

        cartCrash [] (p,c) = Nothing
        cartCrash ((p',c'):pcs) (p,c) = case compare p' p of
                                          LT -> ((p',c'):) <$> cartCrash pcs (p,c)
                                          EQ -> Just pcs -- Crash!
                                          GT -> Nothing -- no crash

day13b_main :: IO ()
day13b_main = generic_main "../data/13a" day13a_parser day13b_solve show

