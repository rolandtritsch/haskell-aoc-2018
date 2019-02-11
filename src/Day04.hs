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

import Text.Megaparsec (many, manyTill, eof, optional, (<|>), try)
import Text.Megaparsec.Char (newline, char, string, digitChar)
import Text.Megaparsec.Char.Lexer (decimal)

import Util (inputRaw, inputRaw1, inputParser1, Parser)

import Data.Ord (comparing)
import Data.List (sort, maximumBy)
import Data.List.Split (splitOneOf)
import qualified Data.Map as M

type Id = Int
type Minute = Int
data Date = Date String Int Int deriving (Show, Eq)

-- | just to process the input.
data Event
  = StartShift Id Date
  | StateChange Date
  deriving (Show, Eq)
type Events = [Event]

data State = Awake | Asleep deriving (Show, Eq)

-- | the data record to show if a guard is awake or asleep
-- on a given date in a given minute.
data Record = Record Id Date Minute State deriving (Show, Eq)
type Records = [Record]

-- | all shifts by guard.
type Shifts = M.Map Id Records

-- | read the input file
input :: Events
input = (input2Stream . sort . inputRaw) "input/Day04input.txt"

-- | turn input into event stream.
input2Stream :: [String] -> Events
input2Stream = map event where
  event l
    | isStartOfShift = StartShift (read $ tokens !! 7) date
    | otherwise = StateChange date
    where
      -- e.g. [1518-11-22 00:00] Guard #1231 begins shift
      -- e.g. [1518-04-13 00:00] falls asleep
      -- e.g. [1518-04-06 00:58] wakes up
      tokens = splitOneOf "[]:# " l
      isStartOfShift
        | tokens !! 5 == "Guard" = True
        | otherwise = False
      date = Date (tokens !! 1) (read $ tokens !! 2) (read $ tokens !! 3)

-- | read the input file
input1 :: String
input1 = inputRaw1 "input/Day04input.txt"

-- | the parsed input.
parsedInput :: Events
parsedInput = inputParser1 parseEvents (unlines $ sort $ inputRaw "input/Day04input.txt")

parseEvents :: Parser Events
parseEvents = manyTill (parseEvent <* optional newline) eof

parseEvent :: Parser Event
parseEvent = try parseShiftStart <|> parseStateChange

parseShiftStart, parseStateChange :: Parser Event
parseShiftStart = toShift
  <$> parseDate
  <*> parseGuardId
  where
    toShift date id' = StartShift id' date
parseStateChange = StateChange <$> parseDate <* (string "falls asleep" <|> string "wakes up")

parseGuardId :: Parser Id
parseGuardId = string "Guard #" *> decimal <* string " begins shift"

parseDate :: Parser Date
parseDate = Date
  <$ char '['
  <*> many (digitChar <|> (char '-'))
  <* char ' '
  <*> decimal
  <* char ':'
  <*> decimal
  <* string "] "

-- | process the input stream. (Obviously/Naturally) We always start with a
-- StartShift event and keep on reading events until there are none left over.
-- The *difference* (in minutes) between the previous event and the current
-- event will tell us how many records we have to generate (to cover the number
-- of minutes the guard was in the given state (Asleep/Awake)).
stream2Record :: Events -> Records
stream2Record ((StartShift firstGid firstDate):events) = go firstGid firstDate 0 Awake events where
  go gid date prevMinute state ((StartShift nextGid nextDate):remainingEvents) = (records gid date prevMinute 59 state) ++ go nextGid nextDate 0 Awake remainingEvents
  go gid date prevMinute state ((StateChange (Date _ _ minute)):remainingEvents) = (records gid date prevMinute (minute-1) state) ++ go gid date minute (flip' state) remainingEvents
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
    sumOfMins = (length . filter (\(Record _ _ _ s) -> (==) s Asleep))

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
