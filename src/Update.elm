module Update exposing (..)

import List exposing (head, length, map)
import Maybe exposing (withDefault)
import Model exposing (FillBalls, Frame(..), Model, emptyModel)
import String exposing (toInt)
import Utils exposing (removeHead)

type Msg
  = Change String
  | Reset
  | DeleteLast

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent -> if model.isRoundComplete
                           then model
                           else
                             if length model.frameList < 10
                               then handleStringInput model newContent
                               else if length model.frameList == 10
                                    then handleLastFrame model newContent
                                    else model -- there cannot be more than 10 frames
    Reset -> emptyModel
    DeleteLast -> {model |
                    frameList = (removeHead model.frameList)
                    , fillBalls = (Nothing, Nothing)
                    , lastFrame = withDefault (OpenFrame 0 0) (head (removeHead model.frameList))
                    , isRoundComplete = False}

handleStringInput: Model -> String -> Model
handleStringInput model newContent =
  case newContent of
    "x" -> case model.lastFrame of
             Strike -> addStrike model
             Spare _ -> addStrike model
             OpenFrame _ _ -> addStrike model
             HalfFrame _ -> model -- this case is an input error, ignore it
    _ -> handleDigitInput model newContent

handleDigitInput: Model -> String -> Model
handleDigitInput model strDigit =
  case toInt strDigit of
    Just currentRoll -> case model.lastFrame of
                          Strike -> addHalfFrame model currentRoll
                          Spare _ -> addHalfFrame model currentRoll
                          OpenFrame _ _ -> addHalfFrame model currentRoll
                          HalfFrame lastRoll -> handleHalfFrame model lastRoll currentRoll
    Nothing -> model -- input error, ignore it

handleLastFrame: Model -> String -> Model
handleLastFrame model newContent =
  let
    updateFillBalls: Int -> FillBalls
    updateFillBalls roll =
      case model.fillBalls of
        (Nothing, Nothing) -> (Just roll, Nothing)
        (Just previousRoll, Nothing) -> (Just previousRoll, Just roll)
        (_, _) -> model.fillBalls

    isFillBallsFull: FillBalls -> Bool
    isFillBallsFull fillBalls =
      case fillBalls of
        (Just _, Just _) -> True
        (_, _) -> False

    onLastFrameStrike: Int -> Model
    onLastFrameStrike currentRoll =
      Debug.log "FILL BALL" { model | fillBalls = updateFillBalls currentRoll, isRoundComplete = if isFillBallsFull (updateFillBalls currentRoll) then True else False}

    onLastFrameSpare: Int -> Model
    onLastFrameSpare currentRoll =
      Debug.log "FILL BALL" { model | fillBalls = updateFillBalls currentRoll, isRoundComplete = True }
  in
    case newContent of
      "x" -> case model.lastFrame of -- handle "x" input
               Strike -> onLastFrameStrike 10
               Spare _ -> onLastFrameSpare 10
               HalfFrame _ -> model -- this case is an input error, ignore it
               OpenFrame _ _ -> model -- this case is an input error, ignore it
      _ -> case toInt newContent of -- handle digit input
             Just currentRoll -> case model.lastFrame of
                                   Strike -> onLastFrameStrike currentRoll
                                   Spare _ -> onLastFrameSpare currentRoll
                                   HalfFrame lastRoll -> handleHalfFrame model lastRoll currentRoll
                                   OpenFrame _ _ -> model
             Nothing -> model -- input error, ignore it

handleHalfFrame: Model -> Int -> Int -> Model
handleHalfFrame model lastRoll currentRoll =
  if lastRoll + currentRoll < 10
    then addOpenFrame model lastRoll currentRoll
    else if lastRoll + currentRoll == 10
           then addSpare model lastRoll
           else model -- you can't hit more than 10 pins, input error, igore it


addStrike: Model -> Model
addStrike model = Debug.log "STRIKE" { model | frameList = Strike::model.frameList, lastFrame = Strike }

addSpare: Model -> Int -> Model
addSpare model lastRoll =
  Debug.log "SPARE" { model |
    frameList = (Spare lastRoll)::(removeHead model.frameList)
    , lastFrame = Spare lastRoll }

addHalfFrame: Model -> Int -> Model
addHalfFrame model lastRoll = Debug.log "HALF FRAME" { model | frameList = (HalfFrame lastRoll)::model.frameList, lastFrame = HalfFrame lastRoll }

addOpenFrame: Model -> Int -> Int -> Model
addOpenFrame model a b =
  Debug.log "OPEN FRAME" { model |
    frameList = (OpenFrame a b)::(removeHead model.frameList)
    , lastFrame = OpenFrame a b }

computeScore: List Int -> List Int
computeScore frames =
  case frames of
    [] -> []
    x::y::z::zs -> if x == 10
                     then (x + y + z)::(computeScore (y::z::zs))
                     else
                       if x + y == 10
                         then (x + y + z)::(computeScore (z::zs))
                         else (x + y)::(computeScore (z::zs))
    x::y::ys -> if x == 10
                  then 0::(computeScore (y::ys))
                  else
                    if x + y == 10
                      then 0::(computeScore (y::ys))
                      else (x + y)::(computeScore ys)
    _::_ -> [0]

flattenModel: List Frame -> FillBalls -> List Int
flattenModel frames fillBalls =
  let
    flattenFrames: List Frame -> List Int
    flattenFrames list =
      case list of
          [] -> []
          x::xs -> case x of
                     Strike -> 10::flattenFrames xs
                     Spare roll -> roll::(10 - roll)::flattenFrames xs
                     OpenFrame roll1 roll2 -> roll1::roll2::flattenFrames xs
                     HalfFrame roll -> roll::flattenFrames xs

    flattenFillBalls: FillBalls -> List Int
    flattenFillBalls pair =
      case pair of
        (Just roll1, Just roll2) -> [roll1, roll2]
        (Just roll1, Nothing) -> [roll1]
        (_, _) -> []
  in
    flattenFrames frames ++ flattenFillBalls fillBalls

totalScore: List Int -> List Int
totalScore list =
  case list of
    [] -> []
    x::xs -> x :: map (\y -> x + y) (totalScore xs)
