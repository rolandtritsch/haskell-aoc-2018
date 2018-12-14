module Day14Spec where

import Test.Hspec

import Day14
import qualified Day14.Part1 as D14P1
import qualified Day14.Part2 as D14P2

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "should return the input" $ do
      input `shouldBe` "607331"

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D14P1.solve "5" `shouldBe` "0124515891"
      D14P1.solve "9" `shouldBe` "5158916779"
      D14P1.solve "18" `shouldBe` "9251071085"
      D14P1.solve "2018" `shouldBe` "5941429882"

    it "should solve the puzzle" $ do
      D14P1.solve input `shouldBe` "8610321414"

  describe "solve - Part2" $ do
    it "should return the right result(s) for the testcases" $ do
      D14P2.solve "01245" `shouldBe` 5
      D14P2.solve "51589" `shouldBe` 9
      D14P2.solve "92510" `shouldBe` 18
      D14P2.solve "59414" `shouldBe` 2018

    --it "should solve the puzzle" $ do
      --D14P2.solve input `shouldBe` 2
