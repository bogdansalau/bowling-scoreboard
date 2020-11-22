module Model exposing (..)

import String exposing (fromInt)
type Frame = Strike | Spare Int | HalfFrame Int | OpenFrame Int Int
type alias FillBalls = (Maybe Int, Maybe Int)

type alias Model = {
  frameList: List Frame,
  fillBalls: FillBalls,
  lastFrame: Frame,
  isRoundComplete: Bool
  }

init: Model
init = emptyModel

frameToString: Frame -> String
frameToString frameResult =
  case frameResult of
    Strike -> "X"
    Spare x -> fromInt x ++ "/"
    HalfFrame x -> fromInt x
    OpenFrame x y -> fromInt x ++ " " ++ fromInt y

fillBallToString: FillBalls -> String
fillBallToString fillBalls =
  let
    computeValue roll = if roll == 10 then "X" else fromInt roll
  in
    case fillBalls of
      (Just roll1, Just roll2) -> computeValue roll1 ++ " " ++ computeValue roll2
      (Just roll1, Nothing) -> computeValue roll1
      (_, _) -> ""

emptyModel = {frameList = [], fillBalls = (Nothing, Nothing), lastFrame =  OpenFrame 0 0, isRoundComplete = False}
