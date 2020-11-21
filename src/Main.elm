module Main exposing (..)

import Browser
import Model exposing (Model, init)
import Update exposing (update)
import View exposing (view)

main =
  Browser.sandbox { init = init, update = update, view = view }


