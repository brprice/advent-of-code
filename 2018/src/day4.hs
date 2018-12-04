import Data.Function (on)
import Data.List
import qualified Data.Map as M

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
day4a_data :: IO [(Date,Time,Event)]
day4a_data = sortOn (\(d,t,_) -> (d,t)) <$> map parse <$> lines <$> readFile "../data/4a"
  where parse ('[':l) = let (y,'-':l1) = splitAt 4 l
                            (m,'-':l2) = splitAt 2 l1
                            (d,' ':l3) = splitAt 2 l2
                            (hh,':':l4) = splitAt 2 l3
                            (mm,']':' ':event) = splitAt 2 l4
                        in (Date (read y) (read m) (read d)
                           ,Time (read hh) (read mm)
                           ,ev event)
        ev "falls asleep" = FallAsleep
        ev "wakes up" = WakeUp
        ev s = case words s of
                 ["Guard",'#':id,"begins","shift"] -> BeginShift (read id)

day4a_main :: IO ()
day4a_main = print =<< day4a_solve <$> day4a_data

-- record what intervals asleep for
data ShiftInt = ShiftInt Int [((Date,Time),(Date,Time))]
  deriving Show

-- but always only asleep between 00:00 and 00:59, and shifts less than a day, so convert to list of which minutes
data ShiftMins = ShiftMins {guard :: Int, slept :: [Int]}
  deriving Show

-- assume: always awake by time next guard begins shift
-- always awake at start of shift
-- thus structure is (BeginShift g, FallAsleep, WakeUp , FallAsleep ... WakeUp) repeated
shiftInts :: [(Date,Time,Event)] -> [ShiftInt]
shiftInts ((d,t,BeginShift g):ds) = go g [] ds
  where go g slept ((d,t,BeginShift g'):ds) = ShiftInt g slept : go g' [] ds
        go g slept ((d,t,FallAsleep):(d',t',WakeUp):ds) = go g (((d,t),(d',t')):slept) ds
        go g slept [] = [ShiftInt g slept]

shiftMins :: [ShiftInt] -> [ShiftMins]
shiftMins = map go
  where go (ShiftInt g []) = ShiftMins g []
        go (ShiftInt g ints) =
          -- test assumptions
          let startDate = fst $ fst $ head ints
              endDate = fst $ snd $ last ints
              startHour = hour $ snd $ fst $ head ints
              endHour = hour $ snd $ snd $ last ints
          in if startDate /= endDate || startHour /= 0 || endHour /= 0
             then error "AAARGH"
             else ShiftMins g $ concatMap (\((_,ts),(_,tf)) -> [minute ts .. minute tf -1]) ints

-- maps guard to list of #times asleep in minutes 0..59
guardSleepFreq :: [ShiftMins] -> M.Map Int [Int]
guardSleepFreq ss = M.fromListWith (zipWith (+)) $ map (\(ShiftMins g slept) -> (g,toIndicator slept)) ss
  where toIndicator slept = [if elem m slept then 1 else 0 | m<-[0..59]]


day4a_solve :: [(Date,Time,Event)] -> Int
day4a_solve ds = sleepiestGuard * sleepiestMinute
  where sleepMins = guardSleepFreq $ shiftMins $ shiftInts ds
        (sleepiestGuard,slept) = maximumBy (compare`on`(sum.snd)) $ M.assocs sleepMins
        sleepiestMinute = snd $ maximum $ zipWith (,) slept [0..59]


-- 4b: Given records of when guards were asleep, find the pair (g,m) of guard and minute
-- such that that guard is asleep on that minute at least as often than any guard on any minute
-- then return g*m
day4b_data :: IO [(Date,Time,Event)]
day4b_data = day4a_data

day4b_main :: IO ()
day4b_main = print =<< day4b_solve <$> day4b_data

--day4b_solve :: [(Date,Time,Event)] -> Int
day4b_solve ds = g*m
  where sleepMins = guardSleepFreq $ shiftMins $ shiftInts ds
        (f,(g,m)) = maximum $ concatMap (\(g,fs) -> map (\(f,m)->(f,(g,m))) $ zip fs [0..59]) $ M.assocs sleepMins
