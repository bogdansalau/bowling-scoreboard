module Update exposing (..)

import List exposing (length)
import Model exposing (Frame(..), Model, emptyModel)
import String exposing (toInt)
import Utils exposing (removeHead)
type Msg
  = Change String
  | Reset

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent -> if length model.frameList < 10
                           then handleChange model newContent
                           else model
    Reset -> emptyModel

handleChange: Model -> String -> Model
handleChange model newContent =
  case newContent of
    "x" -> case model.lastHit of
             Strike -> addStrike model
             Spare _ -> addStrike model
             OpenFrame _ _ -> addStrike model
             HalfFrame _ -> model -- this case is an input error, ignore it
    _ -> handleDigit model newContent

handleDigit: Model -> String -> Model
handleDigit model strDigit =
  case toInt strDigit of
    Just currentRoll -> case model.lastHit of
                Strike -> addHalfFrame model currentRoll
                Spare _ -> addHalfFrame model currentRoll
                HalfFrame lastRoll -> if lastRoll + currentRoll < 10
                                        then addOpenFrame model lastRoll currentRoll
                                        else if lastRoll + currentRoll == 10
                                               then addSpare model lastRoll
                                               else model -- you can't hit more than 10 pins, input error, igore it
                OpenFrame _ _ -> addHalfFrame model currentRoll
    Nothing -> model

addStrike: Model -> Model
addStrike model = Debug.log "STRIKE" { model | frameList = Strike::model.frameList, lastHit = Strike }

addSpare: Model -> Int -> Model
addSpare model lastRoll =
  Debug.log "SPARE" { model |
    frameList = (Spare lastRoll)::(removeHead model.frameList)
    , lastHit = Spare lastRoll }

addHalfFrame: Model -> Int -> Model
addHalfFrame model lastRoll = Debug.log "HALF FRAME" { model | frameList = (HalfFrame lastRoll)::model.frameList, lastHit = HalfFrame lastRoll }

addOpenFrame: Model -> Int -> Int -> Model
addOpenFrame model a b =
  Debug.log "OPEN FRAME" { model |
    frameList = (OpenFrame a b)::(removeHead model.frameList)
    , lastHit = OpenFrame a b }
