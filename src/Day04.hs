{-|
Problem: <https://adventofcode.com/2018/day/4>

Solution:

General - Tough.

First question: What is the best data structure to read-in the problem?
At the end you realize that the date is not needed to solve the problems.
It is only needed to sort/order the input lines.

I then decided to turn every line into an event and then processes the
resulting (ordered) event stream to produce records from it. Every
record shows when a guard was asleep (per day/shift). Here it is
important to realize that you need to represent the minutes that he/she
is asleep in a way that will allow you to solve the problems. I decided
to generate a *mask* for the hour that the guard is on duty. Every minute
is either on (True; means asleep) or off (False; awake).

With that I can start to aggregate more data structures.

First I can group the records by guard. That will give me all days
that the guard was on duty (and all the minutes he was asleep)).
This can then be agreegated further.

Part 1 - Look for the guard that slept the most. And then for the
minute he/she slept the most.

Part 2 - Create a/the list of (guard, (min, minsAsleep)) and find
the maximum of minsAsleep (and return the guard and the min.
-}
module Day04 where

import Data.Ord (comparing)
import Data.List (sort, repeat, groupBy, maximumBy, transpose)
import Data.List.Split (splitOneOf)

import Util (inputRaw)

type Id = Int
type Minute = Int
type MinutesAsleep = [Bool]

-- | just to process the input.
data Event
  = StartShift Id
  | FallAsleep Minute
  | Wakeup Minute
  deriving (Show, Eq)

-- | the data record to show how much a guard is asleep (one record per day).
data Record = Record Id MinutesAsleep deriving (Show, Eq, Ord)

-- | read the input file
input :: [String]
input = sort $ inputRaw "input/Day04input.txt"

-- | turn input into event stream.
input2Stream :: [String] -> [Event]
input2Stream ls = map event ls where
  event l
    | isStartOfShift = StartShift (read $ tokens !! 7)
    | isFallingAsleep = FallAsleep (read $ tokens !! 3)
    | isWakingUp = Wakeup (read $ tokens !! 3)
    | otherwise = error "Do not know what to do."
    where
      -- e.g. [1518-11-22 00:00] Guard #1231 begins shift
      -- e.g. [1518-04-13 00:00] falls asleep
      -- e.g. [1518-04-06 00:58] wakes up
      tokens = splitOneOf "[]:# " l
      isStartOfShift
        | tokens !! 5 == "Guard" = True
        | otherwise = False
      isFallingAsleep
        | tokens !! 5 == "falls" = True
        | otherwise = False
      isWakingUp
        | tokens !! 5 == "wakes" = True
        | otherwise = False

-- | process the input stream. Use a recursion to group/collect all
-- events for the record.
stream2Record :: [Event] -> [Record]
stream2Record ((StartShift firstGid):remainingEvents) = go firstGid awake remainingEvents where
  awake = take 60 $ repeat False
  go gid minsAsleep ((StartShift gid'):remainingEvents') = [Record gid minsAsleep] ++ go gid' awake remainingEvents'
  go gid minsAsleep ((FallAsleep minute):remainingEvents') = go gid (fallsAsleep True minute minsAsleep) remainingEvents'
  go gid minsAsleep ((Wakeup minute):remainingEvents') = go gid (fallsAsleep False minute minsAsleep) remainingEvents'
  go gid minsAsleep [] = [Record gid minsAsleep]
  fallsAsleep state mins minsAsleep = take mins minsAsleep ++ (take (60-mins) $ repeat state)
stream2Record _ = error "Unexpected match in event stream."

-- | group the mins asleep by guard/gid.
groupMinsAsleepByGid :: [Record] -> [(Id, [MinutesAsleep])]
groupMinsAsleepByGid rs = map (foldl collectMinsAsleep (0, [])) $ groupBy byGid $ sort rs where
  byGid (Record gid _) (Record gid' _) = gid == gid'
  collectMinsAsleep (_, accumulator) (Record gid minsAsleep) = (gid, accumulator ++ [minsAsleep])

-- | which guard is most asleep.
mostAsleep :: [Record] -> (Id, Int)
mostAsleep rs = maximumBy (comparing snd) $ map sumUpMins (groupMinsAsleepByGid rs) where
  sumUpMins (gid, minsAsleep) = (gid, foldl (\s ms -> s + (length $ filter id ms)) 0 minsAsleep)

-- | do a/the histogram (by mins) for a given list of minutes asleep.
-- The trick here is to realize that getting the histogramPerMinute
-- is easy after you collect all minsAsleep for the given gid and
-- *just* transpose them (turn every col into a row).
histogram :: [MinutesAsleep] -> [(Int, Int)]
histogram minsAsleep = zip [0 ..] $ map (length . filter id) $ transpose minsAsleep

-- | which minute is the given guard asleep the most (min, minsasleep).
asleepMost :: [Record] -> Id -> (Int, Int)
asleepMost rs gid = maximumBy (comparing snd) $ histogram minsAsleep where
  minsAsleep = map (\(Record _ ms) -> ms) $ filter (byGid gid) rs where
    byGid gid' (Record gid'' _) = gid' == gid''

-- | combine the previous two functions to get [(gid, (min, minsasleep)].
-- Then we just have to look for the max minasleep.
mostAsleepGidMinute :: [Record] -> (Id, (Int, Int))
mostAsleepGidMinute rs = maximumBy byMinAsleep $ concatMap processHistograms $ map processGroups $ groupMinsAsleepByGid rs where
  byMinAsleep (_, (_, minsAsleep)) (_, (_, minsAsleep'))
    | minsAsleep == minsAsleep' = EQ
    | minsAsleep < minsAsleep' = LT
    | minsAsleep > minsAsleep' = GT
  byMinAsleep _ _ = error "Unexpected pattern match"
  processGroups (gid, minsAsleep) = (gid, (histogram minsAsleep))
  processHistograms (gid, h:rest)
    | null rest = [(gid, h)]
    | otherwise = [(gid, h)] ++ processHistograms (gid, rest)
  processHistograms _ = error "Unexpected pattern match"
