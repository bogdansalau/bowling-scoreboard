module Model exposing (..)

import String exposing (fromInt)
type Frame = Strike | Spare Int | HalfFrame Int | OpenFrame Int Int
type FillBall = FillBall (Maybe Int, Maybe Int)

type alias Model = {
  frameList: List Frame,
  fillBall: FillBall
  }

init: Model
init = testModel

frameToString: Frame -> String
frameToString frameResult =
  case frameResult of
    Strike -> "X"
    Spare x -> fromInt x ++ "/"
    HalfFrame x -> fromInt x
    OpenFrame x y -> fromInt x ++ " " ++ fromInt y

fillBallToString: FillBall -> String
fillBallToString fillBall =
  case fillBall of
    FillBall (Just x, Just y) -> fromInt x ++ " " ++ fromInt y
    FillBall (Just x, Nothing) -> fromInt x
    FillBall (_, _) -> ""


testModel = {
  frameList = [
      Strike,
      Spare 7,
      OpenFrame 7 2,
      Spare 9,
      Strike,
      Strike,
      Strike,
      OpenFrame 2 3,
      Spare 6,
      Spare 7
    ],
  fillBall = FillBall (Just 3, Nothing)
  }
