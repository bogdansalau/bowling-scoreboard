module Update exposing (..)

import List exposing (length)
import Model exposing (Frame(..), Model, emptyModel)
import String exposing (toInt)
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
    "x" ->  hitStrike model
    _ -> handleDigit model newContent

handleDigit: Model -> String -> Model
handleDigit model strDigit =
  case toInt strDigit of
    Just x -> halfFrame model x

    Nothing -> model

hitStrike: Model -> Model
hitStrike model = Debug.log "STRIKE" { model | frameList = Strike::model.frameList }

hitSpare: Model -> Model
hitSpare model = model

halfFrame: Model -> Int -> Model
halfFrame model x = Debug.log "STRIKE" { model | frameList = (HalfFrame x)::model.frameList }

openFrame: Model -> Model
openFrame model = model
