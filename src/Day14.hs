{-|
Problem: <https://adventofcode.com/2018/day/14>

Solution:

General - Part 1 was ok. But Part 2 was hard(er) (because
I (initially) was doing the tails on every recipe (over
and over again) and that took way too long).

Part 1 - Make the chocolate ... until we have enough.

Part 2 - Make the chocolate ... until we find alcohol
(and then we call it gluehwein :)).
-}
module Day14 where

import Data.List (tails, findIndex)
import Data.Maybe (fromJust)
import qualified Data.Sequence as S

import Util (inputRaw)

type Score = Int
type Position = Int
data Recipes = Recipes Position Position (S.Seq Score) deriving (Show, Eq)

-- | read the input file
input :: String
input = (head . inputRaw) "input/Day14input.txt"

-- | "cooking" the recipes :). Take the recipes and make new ones from it.
cook :: Recipes -> Recipes
cook (Recipes elfA elfB scores) = recipes' where
  recipes' = Recipes elfA' elfB' scores' where
    scores'
      | sumOfScores >= 10 = scores S.>< S.fromList [firstDigit, secondDigit]
      | otherwise = scores S.>< S.fromList [secondDigit]
      where
        sumOfScores = (S.index scores elfA) + (S.index scores elfB)
        firstDigit = mod (div sumOfScores 10) 10
        secondDigit = mod sumOfScores 10
    elfA' = mod (elfA + 1 + (S.index scores elfA)) (S.length scores')
    elfB' = mod (elfB + 1 + (S.index scores elfB)) (S.length scores')

-- | make the chocolate. By cooking the recipes until we have "enough"
chocolate :: Int -> Recipes
chocolate target = go (Recipes 0 1 (S.fromList [3, 7])) where
  go recipes'@(Recipes _ _ scores)
    | (length scores) >= target + 10 = recipes'
    | otherwise = go (cook recipes')

-- | make the chocolate. Forever ...
chocolate' :: [Int]
chocolate' = 3 : 7 : go (Recipes 0 1 (S.fromList [3, 7])) where
  go recipes@(Recipes _ _ scores) = added ++ go recipes' where
    recipes'@(Recipes _ _ scores') = cook recipes
    added
      | S.length scores + 1 == S.length scores' = [S.index scores' (S.length scores' - 1)]
      | S.length scores + 2 == S.length scores' = [S.index scores' (S.length scores' - 2), S.index scores' (S.length scores' - 1)]
      | otherwise = error "added: Unexpected pattern match."

-- | get a/the hint from the recipes.
hint :: Int -> Recipes -> String
hint target (Recipes _ _ scores) = concatMap collectDigits [0..9] where
  collectDigits n = show (S.index scores (target + n))

-- | make the gluehwein. By cooking the recipes until we find "alcohol" :).
gluehwein :: [Int] -> Int
gluehwein target = go rs Nothing where
  target' = S.fromList target
  rs = iterate cook (Recipes 0 1 (S.fromList [3, 7]))
  go ((Recipes _ _ scores):rs') Nothing = go rs' findAlcohol where
    findAlcohol = S.findIndexL match (S.tails scores) where
      match s = target' == S.take (S.length target') s
  go _ (Just i) = i
  go [] _ = error "gluehwein: Unexected pattern match."

gluehwein' :: [Int] -> Int
gluehwein' target = fromJust $ findIndex match $ tails chocolate' where
  match s = target == take (length target) s
