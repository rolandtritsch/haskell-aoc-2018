module Day10Spec where

import Test.Hspec

import Day10
import qualified Day10.Part1 as D10P1
import qualified Day10.Part2 as D10P2

run :: IO ()
run = hspec $ do
  let testInput = [(( 9, 1),( 0, 2)),(( 7, 0),(-1, 0)),(( 3,-2),(-1, 1)),(( 6,10),(-2,-1)),(( 2,-4),( 2, 2)),((-6,10),( 2,-2)),(( 1, 8),( 1,-1)),(( 1, 7),( 1, 0)),((-3,11),( 1,-2)),(( 7, 6),(-1,-1)),((-2, 3),( 1, 0)),((-4, 3),( 2, 0)),((10,-3),(-1, 1)),(( 5,11),( 1,-2)),(( 4, 7),( 0,-1)),(( 8,-2),( 0, 1)),((15, 0),(-2, 0)),(( 1, 6),( 1, 0)),(( 8, 9),( 0,-1)),(( 3, 3),(-1, 1)),(( 0, 5),( 0,-1)),((-2, 2),( 2, 0)),(( 5,-2),( 1, 2)),(( 1, 4),( 2, 1)),((-2, 7),( 2,-2)),(( 3, 6),(-1,-1)),(( 5, 0),( 1, 0)),((-6, 0),( 2, 0)),(( 5, 9),( 1,-2)),((14, 7),(-2, 0)),((-3, 6),( 2,-1))]

  -- "HI"
  let testMessage = [
        "*   *  ***",
        "*   *   * ",
        "*   *   * ",
        "*****   * ",
        "*   *   * ",
        "*   *   * ",
        "*   *   * ",
        "*   *  ***"
        ]

  -- "GFNKCGGH"
  let message = [
        " ****   ******  *    *  *    *   ****    ****    ****   *    *",
        "*    *  *       **   *  *   *   *    *  *    *  *    *  *    *",
        "*       *       **   *  *  *    *       *       *       *    *",
        "*       *       * *  *  * *     *       *       *       *    *",
        "*       *****   * *  *  **      *       *       *       ******",
        "*  ***  *       *  * *  **      *       *  ***  *  ***  *    *",
        "*    *  *       *  * *  * *     *       *    *  *    *  *    *",
        "*    *  *       *   **  *  *    *       *    *  *    *  *    *",
        "*   **  *       *   **  *   *   *    *  *   **  *   **  *    *",
        " *** *  *       *    *  *    *   ****    *** *   *** *  *    *"
        ]

  describe "input" $ do
    it "should return the input" $ do
      head input `shouldBe` ((-10166,-20343),(1,2))

  describe "paint" $ do
    it "should return the night sky" $ do
      paint ((iterate tick testInput) !! 3) `shouldBe` testMessage
      paint ((iterate tick input) !! 10274) `shouldBe` message

  describe "solve - Part1" $ do
    it "should return the right result(s) for the testcases" $ do
      D10P1.solve testInput `shouldBe` testMessage

    it "should solve the puzzle" $ do
      D10P1.solve input `shouldBe` message

  describe "solve - Part2" $ do
    it "should solve the puzzle" $ do
      D10P2.solve input `shouldBe` 10274
