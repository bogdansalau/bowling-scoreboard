module Update exposing (..)

import Model exposing (Model)
type Msg
  = Change String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent -> model
      --{ model | content = newContent }
