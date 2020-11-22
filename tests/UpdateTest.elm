module UpdateTest exposing (..)

import Expect exposing (Expectation)
import List exposing (repeat)
import Model exposing (Frame(..))
import Test exposing (..)
import Update exposing (..)

genFlattenModelTest name frames expected =
  test name <|
    \_ -> Expect.equal expected (flattenModel (Nothing, Nothing) frames)

genFlattenModelWithFillBallsTest name frames fillBalls expected =
  test name <|
    \_ -> Expect.equal expected (flattenModel fillBalls frames)

genScoreTest name rolls expected =
  test name <|
    \_ -> Expect.equal expected <| computeScore rolls

genTotalScoreTest name rolls expected =
  test name <|
    \_ -> Expect.equal expected <| totalScore <| computeScore rolls

suite : Test
suite =
  describe "Upgrade" [
    describe "flattenModel test" [
      genFlattenModelTest "flatten Strike returns 10-roll" [Strike] [10],
      genFlattenModelTest "flatten Spare returns two rolls summed up to 10" [Spare 3] [3, 7],
      genFlattenModelTest "flatten Open Frame returns two rolls" [OpenFrame 3 3] [3, 3],
      genFlattenModelTest "flatten Half Frame returns one roll" [HalfFrame 5] [5],
      genFlattenModelWithFillBallsTest "flatten Strike with one fill ball" [Strike] (Just 5, Nothing) [10, 5],
      genFlattenModelWithFillBallsTest "flatten Strike with two fill balls" [Strike] (Just 5, Just 10) [10, 5, 10],
      genFlattenModelWithFillBallsTest "flatten Spare with one fill ball" [Spare 6] (Just 5, Nothing) [6, 4, 5]
    ],
    describe "computeScore test" [
      genScoreTest "straight 0 rolls returns score 0 on each frame" (repeat 20 0) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      genScoreTest "perfect game returns score 30 on each frame" (repeat 12 10) [30, 30, 30, 30, 30, 30, 30, 30, 30, 30],
      genScoreTest "straight 1 rolls returns score 2 on each frame" (repeat 20 1) [2, 2, 2, 2, 2, 2, 2, 2, 2, 2],
      genScoreTest "random score 1" testScore1 [20, 17, 9, 20, 30, 22, 15, 5, 17, 13],
      genScoreTest "random score 2" testScore2 [22, 15, 5, 8, 20, 20, 20, 20, 20, 30]
    ],
    describe "totalScore test" [
      genTotalScoreTest "straight 0 rolls returns score 0 on each partial sum" (repeat 20 0) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      genTotalScoreTest "perfect game increases each frame total by 30" (repeat 12 10) [30, 60, 90, 120, 150, 180, 210, 240, 270, 300],
      genTotalScoreTest "straight 1 rolls increase each frame total by 2" (repeat 20 1) [2, 4, 6, 8, 10, 12, 14, 16, 18, 20],
      genTotalScoreTest "random score 1 total" testScore1 [20, 37, 46, 66, 96, 118, 133, 138, 155, 168],
      genTotalScoreTest "random score 2 total" testScore2 [22, 37, 42, 50, 70, 90, 110, 130, 150, 180]
    ]
  ]

testScore1 = [10, 7, 3, 7, 2, 9, 1, 10, 10, 10, 2, 3, 6, 4, 7, 3, 3]
testScore2 = [10, 10, 2, 3, 5, 3, 5, 5, 10, 5, 5, 10, 1, 9, 10, 10, 10]
