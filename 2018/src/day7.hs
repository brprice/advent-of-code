import Data.Bifunctor
import Data.Char
import Data.List

import Text.Megaparsec
import Text.Megaparsec.Char

import Util

type Id = Char
data Task = Task {tid :: Id, prereq :: [Id]} deriving Show

-- 7a: Given a bunch of tasks with prerequisites,
-- find what order they should be completed (alphabetical ids breaking ties)
day7a_parser :: Parser [(Id,Id)] -- gives A is a prereq for B
day7a_parser = many ((,) <$ string "Step " <*> letterChar <* string " must be finished before step " <*> letterChar <* string " can begin." <* eol)

-- return is sorted alphabetically by task
toTasks :: [(Id,Id)] -> [Task]
toTasks prereqs = map (uncurry Task) $ foldr addPre (map (\i -> (i,[])) allIds) prereqs
  where allIds = nub $ sort $ uncurry (++) $ unzip prereqs
        addPre (p,t) ts = map (\(t',ps) -> if t'==t then (t',p:ps) else (t',ps)) ts

schedTasks :: [Task] -> Either String [Id]
schedTasks [] = pure []
schedTasks ts = do (t,rst) <- liftMayEi ("All tasks are blocked: " ++ show ts) $ findNullPrereq ts
                   let rst' = map (del t) rst
                   sch' <- first (\e -> "After doing task "++show t++", we discovered:\n" ++ e) $ schedTasks rst'
                   pure $ t : sch'
  where findNullPrereq [] = Nothing
        findNullPrereq (Task t []:xs) = pure (t,xs)
        findNullPrereq (x:xs) = do (y,ys) <- findNullPrereq xs
                                   pure $ (y,x:ys)
        del t (Task t' ps) = Task t' $ delete t ps
        liftMayEi _ (Just x) = Right x
        liftMayEi e Nothing = Left e

day7a_solve :: [(Id,Id)] -> Either String [Id]
day7a_solve = schedTasks . toTasks

day7a_main :: IO ()
day7a_main = generic_main "../data/7a" day7a_parser day7a_solve id

-- 7b: Now you have 5 workers, and each task takes time: 'A' will take 61 seconds, 'B' 62 etc

type Time = Integer
type TaskTimed = (Task,Time)

addTiming :: Task -> TaskTimed
addTiming (Task t p) = (Task t p , 60 + fromIntegral (ord t - ord 'A') + 1)

type Worker = Int
data Event = Start Worker Id | Finish Worker Id | AllDone deriving Show

schedTimedTasks :: Int -> [TaskTimed] -> Either String [(Time,Event)]
schedTimedTasks n ts = let (ready,blocked) = partition (null.prereq.fst) ts
                       in stt 0 (map (\(t,d) -> (tid t,d)) ready) blocked [1..n] []
  where -- treat these lists as priority queues
        -- stt readyTasks blockedTasks readyWorkers blockedWorkers = ...
        stt :: Time -> [(Id,Time)] -> [TaskTimed] -> [Worker] -> [(Time,Worker,Id)] -> Either String [(Time,Event)]
        stt t rT bT [] [] = Left $ unwords ["Error: All my workers have wandered off...:",show t,show rT,show bT]
        stt t [] [] _ [] = pure [(t,AllDone)]
        stt t [] bT rW [] = Left $ unwords ["Error: cannot make progress:"
                                           ,"no tasks are either ready or in progress:"
                                           ,show t
                                           ,show bT
                                           ,show rW]
        stt t ((k,d):ks) bT (w:rWs) bW = ((t,Start w k) :) <$> (stt t ks bT rWs $ insert (t+d,w,k) bW)
        stt _ rT bT [] ((t',w,k):bW) = ((t',Finish w k) :)
                                   <$> let (rT',bT') = finishTask k rT bT
                                       in stt t' rT' bT' [w] bW
        stt _ [] bT rW ((t',w,k):bW) = ((t',Finish w k) :)
                                   <$> let (rT',bT') = finishTask k [] bT
                                       in stt t' rT' bT' (insert w rW) bW

        finishTask k r b = let (r',b') = partition (null.prereq.fst) $ map (del k) b
                           in (merge r $ map (\(t,d) -> (tid t,d)) r', b')
        del t (Task t' ps,d) = (Task t' $ delete t ps,d)
        merge [] ys = ys
        merge xs [] = xs
        merge (x:xs) (y:ys) | x<=y = x : merge xs (y:ys)
                            | otherwise = y : merge (x:xs) ys

day7b_solve :: [(Id,Id)] -> Either String [(Time,Event)]
day7b_solve = schedTimedTasks 5 . map addTiming . toTasks

day7b_show :: [(Time,Event)] -> String
day7b_show [] = "Error: no events have happened at all!"
day7b_show tes = case last tes of
                   (t,AllDone) -> "All done at time " ++ show t
                   _ -> "The events don't end with AllDone: " ++ show tes

day7b_main :: IO ()
day7b_main = generic_main "../data/7a" day7a_parser day7b_solve day7b_show
