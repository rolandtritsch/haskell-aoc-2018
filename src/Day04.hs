{-|
Problem: <https://adventofcode.com/2018/day/4>

Solution:

General - ???

Part 1 - ???

Part 2 - ???
-}
module Day04 where

import Data.Ord (comparing)
import Data.List (sort, repeat, groupBy, maximumBy, transpose)
import Data.List.Split (splitOneOf)

import Util (inputRaw)

type Id = Int
type Date = String
type MinutesAsleep = [Bool]

data Record = Record Id Date MinutesAsleep deriving (Show, Eq, Ord)

-- | read the input file
input :: [String]
input = sort $ inputRaw "input/Day04input.txt" where

-- | process the input. Use a recursion to group/collect all lines
-- for a given line.
processInput :: [String] -> [Record]
processInput (l:rest) = go (getGid l) (getDate l) awake rest where
  -- e.g. [1518-11-22 00:00] Guard #1231 begins shift
  -- e.g. [1518-04-13 00:00] falls asleep
  -- e.g. [1518-04-06 00:58] wakes up
  tokens l' = splitOneOf "[]:# " l'
  getGid l' = read $ (tokens l') !! 7
  getDate l' = (tokens l') !! 1
  awake = take 60 $ repeat False
  go gid date minsAsleep (l':lines')
    | isStartOfShift = [Record gid date minsAsleep] ++ go (getGid l') (getDate l') awake lines'
    | isFallingAsleep = go gid date (fallsAsleep True) lines'
    | isWakingUp = go gid date (fallsAsleep False) lines'
    | otherwise = error "Do not know what to do."
    where
      isStartOfShift
        | (tokens l') !! 5 == "Guard" = True
        | otherwise = False
      isFallingAsleep
        | (tokens l') !! 5 == "falls" = True
        | otherwise = False
      isWakingUp
        | (tokens l') !! 5 == "wakes" = True
        | otherwise = False
      fallsAsleep state = take mins minsAsleep ++ (take (60-mins) $ repeat state) where
        mins = read $ (tokens l') !! 3
  go gid date minsAsleep [] = [Record gid date minsAsleep]
processInput [] = error "Upppss. Nothing to process."

-- | group the mins asleep by guard/gid.
groupMinsAsleepByGid :: [Record] -> [(Id, [MinutesAsleep])]
groupMinsAsleepByGid rs = map (foldl collectMinsAsleep (0, [])) $ groupBy byGid $ sort rs where
  byGid (Record gid _ _) (Record gid' _ _) = gid == gid'
  collectMinsAsleep (_, accumulator) (Record gid _ minsAsleep) = (gid, accumulator ++ [minsAsleep])

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
  minsAsleep = map (\(Record _ _ ms) -> ms) $ filter (byGid gid) rs where
    byGid gid' (Record gid'' _ _) = gid' == gid''

-- | combine the previous two functions to get [(gid, (min, minsasleep)].
-- Then we just have to look for the max minasleep.
mostAsleepGid :: [Record] -> (Id, (Int, Int))
mostAsleepGid rs = maximumBy byMinAsleep $ concatMap processHistograms $ map processGroups $ groupMinsAsleepByGid rs where
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
