{-|
Problem: <https://adventofcode.com/2018/day/4>

Solution:

General - Tough. Refactored for (better) readability.

Insight 1: After sorting the input lines, they represent an event
stream with a shift starting (with the guard being awake) followed
by a couple of state changes (falling asleep, waking up, ...) until
the next shift starts.

Insight 2: The best data structure actually shows the state of the
guard (awake, asleep) in the minute of the day.

I therefore decided to turn every line into an event and then processes
the resulting (ordered) event stream to produce records from it. Every
record shows when a guard was asleep (per min; if the guard sleeps for
two mins I create two records).

With that I can start to aggregate more data structures.

First I can group the records by guard. That will give me all days
that the guard was on duty (and all the minutes he was asleep :))).
This can then be agreegated further.

Part 1 - Look for the guard that slept the most. And then for the
minute he/she slept the most.

Part 2 - Create a/the list of (guard, (min, minsAsleep)) and find
the maximum of minsAsleep (and return the guard and the min.
-}
module Day04 where

import Data.Ord (comparing)
import Data.List (sort, maximumBy)
import Data.List.Split (splitOneOf)
import qualified Data.Map as M

import Util (inputRaw)

type Id = Int
type Date = String
type Minute = Int

-- | just to process the input.
data Event
  = StartShift Id Date
  | StateChange Minute
  deriving (Show, Eq)

data State = Awake | Asleep deriving (Show, Eq)

-- | the data record to show if a guard is awake or asleep
-- on a given date in a given minute.
data Record = Record Id Date Minute State deriving (Show, Eq)

-- | all shifts by guard.
type Shifts = M.Map Id [Record]

-- | read the input file
input :: [String]
input = sort $ inputRaw "input/Day04input.txt"

-- | turn input into event stream.
input2Stream :: [String] -> [Event]
input2Stream ls = map event ls where
  event l
    | isStartOfShift = StartShift (read $ tokens !! 7) (tokens !! 1)
    | otherwise = StateChange (read $ tokens !! 3)
    where
      -- e.g. [1518-11-22 00:00] Guard #1231 begins shift
      -- e.g. [1518-04-13 00:00] falls asleep
      -- e.g. [1518-04-06 00:58] wakes up
      tokens = splitOneOf "[]:# " l
      isStartOfShift
        | tokens !! 5 == "Guard" = True
        | otherwise = False

-- | process the input stream. (Obviously/Naturally) We always start with a
-- StartShift event and keep on reading events until there are none left over.
-- The *difference* (in minutes) between the previous event and the current
-- event will tell us how many records we have to generate (to cover the number
-- of minutes the guard was in the given state (Asleep/Awake)).
stream2Record :: [Event] -> [Record]
stream2Record ((StartShift firstGid firstDate):events) = go firstGid firstDate 0 Awake events where
  go gid date prevMinute state ((StartShift nextGid nextDate):remainingEvents) = (records gid date prevMinute 59 state) ++ go nextGid nextDate 0 Awake remainingEvents
  go gid date prevMinute state ((StateChange minute):remainingEvents) = (records gid date prevMinute (minute-1) state) ++ go gid date minute (flip' state) remainingEvents
  go gid date prevMinute state [] = records gid date prevMinute 59 state
  records gid date startMin endMin state = [Record gid date m state | m <- [startMin..endMin]]
  flip' Awake = Asleep
  flip' Asleep = Awake
stream2Record _ = error "Unexpected pattern match."

-- | from the records, create a map to hold all shifts (by guard).
record2Shift :: [Record] -> Shifts
record2Shift rs = M.fromListWith (++) $ map (\r@(Record gid _ _ _) -> (gid, [r])) rs

-- | which guard is most asleep.
mostAsleep :: Shifts -> (Id, Int)
mostAsleep shifts = maximumBy (comparing snd) $ M.toList sumOfMinsByGid where
  sumOfMinsByGid = M.map sumOfMins shifts where
    sumOfMins rs = (length . filter (\(Record _ _ _ s) -> (==) s Asleep)) rs

-- | do a histogram of (minOfTheHour, minsAsleep) (for the given list of records).
histogram :: [Record] -> [(Int, Int)]
histogram rs = M.toList $ M.fromListWith (+) $ map (\(Record _ _ m s) -> (m, (state2Int s))) rs where
  state2Int Awake = 0
  state2Int Asleep = 1

-- | which minute is the given guard asleep the most (min, minsasleep).
asleepMost :: Shifts -> Id -> (Int, Int)
asleepMost shifts gid = maximumBy (comparing snd) (histogram (shifts M.! gid))

-- | of all guards, which guard is most frequently asleep on the same minute.
-- We return (guard, (minute, minsAsleep)).
mostAsleepSameMinute :: Shifts -> (Id, (Int, Int))
mostAsleepSameMinute shifts = maximumBy byMinAsleep maxMinAsleepByGid where
  maxMinAsleepByGid = map (\g -> (g, (asleepMost shifts g))) (M.keys shifts)
  byMinAsleep (_, (_, minsAsleep)) (_, (_, minsAsleep')) = compare minsAsleep minsAsleep'
