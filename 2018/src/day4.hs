import Data.Function (on)
import Data.List
import qualified Data.Map as M

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Util

-- 4a: Given records of when guards were asleep, find the sleepiest one, and then their sleepiest minute
-- then return GuardId-of-sleepiest*their-sleepiest-minute
data Date = Date {year, month, day :: Int}
  deriving (Show,Eq,Ord)
data Time = Time {hour, minute :: Int}
  deriving (Show,Eq,Ord)
data Event = BeginShift Int -- int is guard id
           | FallAsleep -- currently on guard falls asleep
           | WakeUp -- currently on guard wakes up
  deriving Show

-- nb: input not date-sorted!
-- assume only one thing happens on each minute
day4a_parser :: Parser [(Date,Time,Event)]
day4a_parser = sortOn (\(d,t,_) -> (d,t)) <$> many (p <* eol)
  where p = (\(d,t) e -> (d,t,e)) <$> between (single '[') (single ']') dateTime <* single ' ' <*> event
        dateTime = (,) <$> date <* single ' ' <*> time
        date = Date <$> decimal <* single '-' <*> decimal <* single '-' <*> decimal
        time = Time <$> decimal <* single ':' <*> decimal
        event = WakeUp <$ string "wakes up"
            <|> FallAsleep <$ string "falls asleep"
            <|> BeginShift <$ string "Guard #" <*> decimal <* string " begins shift"

day4a_main :: IO ()
day4a_main = generic_main "../data/4a" day4a_parser day4a_solve show

-- record what intervals asleep for
data ShiftInt = ShiftInt Int [((Date,Time),(Date,Time))]
  deriving Show

-- but always only asleep between 00:00 and 00:59, and shifts less than a day, so convert to list of which minutes
data ShiftMins = ShiftMins {guard :: Int, slept :: [Int]}
  deriving Show

-- assume: always awake by time next guard begins shift
-- always awake at start of shift
-- thus structure is (BeginShift g, FallAsleep, WakeUp , FallAsleep ... WakeUp) repeated
shiftInts :: [(Date,Time,Event)] -> Either String [ShiftInt]
shiftInts ((d,t,BeginShift g):ds) = go g [] ds
  where go g slept ((d,t,BeginShift g'):ds) = (ShiftInt g slept :) <$> go g' [] ds
        go g slept ((d,t,FallAsleep):(d',t',WakeUp):ds) = go g (((d,t),(d',t')):slept) ds
        go g slept [] = pure [ShiftInt g slept]
        go g slept _ = Left "events are not (Begin(Fall Wake)*)* that we expect"
shiftInts _ = Left "events are not (Begin(Fall Wake)*)* that we expect"

shiftMins :: [ShiftInt] -> Either String [ShiftMins]
shiftMins = mapM go
  where go (ShiftInt g []) = pure $ ShiftMins g []
        go (ShiftInt g ints) =
          -- test assumptions
          let startDate = fst $ fst $ head ints
              endDate = fst $ snd $ last ints
              startHour = hour $ snd $ fst $ head ints
              endHour = hour $ snd $ snd $ last ints
          in if startDate /= endDate || startHour /= 0 || endHour /= 0
             then Left "AAARGH"
             else pure $ ShiftMins g $ concatMap (\((_,ts),(_,tf)) -> [minute ts .. minute tf -1]) ints

-- maps guard to list of #times asleep in minutes 0..59
guardSleepFreq :: [ShiftMins] -> M.Map Int [Int]
guardSleepFreq ss = M.fromListWith (zipWith (+)) $ map (\(ShiftMins g slept) -> (g,toIndicator slept)) ss
  where toIndicator slept = [if elem m slept then 1 else 0 | m<-[0..59]]


day4a_solve :: [(Date,Time,Event)] -> Either String Int
day4a_solve ds = do sis <- shiftInts ds
                    sms <- shiftMins sis
                    let sleepMins = guardSleepFreq sms
                    let (sleepiestGuard,slept) = maximumBy (compare`on`(sum.snd)) $ M.assocs sleepMins
                    let sleepiestMinute = snd $ maximum $ zipWith (,) slept [0..59]
                    pure $ sleepiestGuard * sleepiestMinute

-- 4b: Given records of when guards were asleep, find the pair (g,m) of guard and minute
-- such that that guard is asleep on that minute at least as often than any guard on any minute
-- then return g*m

day4b_main :: IO ()
day4b_main = generic_main "../data/4a" day4a_parser day4b_solve show

day4b_solve :: [(Date,Time,Event)] -> Either String Int
day4b_solve ds = do sis <- shiftInts ds
                    sms <- shiftMins sis
                    let sleepMins = guardSleepFreq sms
                    let (f,(g,m)) = maximum $ concatMap (\(g,fs) -> map (\(f,m)->(f,(g,m))) $ zip fs [0..59]) $ M.assocs sleepMins
                    pure $ g*m
