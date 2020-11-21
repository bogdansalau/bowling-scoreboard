module View exposing (..)

import Debug exposing (toString)
import Html.Attributes exposing (colspan, placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import List exposing (indexedMap, map, reverse)
import Html exposing (Attribute, Html, button, input, table, td, text, th, thead, tr)
import List exposing (range)
import Model exposing (FillBall(..), Frame(..), Model, fillBallToString, frameToString)
import Update exposing (Msg(..))

generateHeader: List (Html msg)
generateHeader = (range 1 10 |> map (\x -> th [style "text-align" "center", style "width" "56px", style "height" "36px"]  [ text (toString x) ]))

frameToResult: Frame -> Html msg
frameToResult frame =
  td [style "border" "2px solid black", style "text-align" "center", style "width" "52px", style "height" "32px"] [ text (frameToString frame) ]

lastFrameToResult: Frame -> FillBall -> Html msg
lastFrameToResult frame fillBall =
  td [style "border" "2px solid black", style "text-align" "center", style "width" "52px", style "height" "32px"] [ text (frameToString frame ++ " " ++ fillBallToString fillBall) ]

handleFrameList: FillBall -> Int -> Frame -> Html msg
handleFrameList fillBall i frame =
  if i == 9
    then lastFrameToResult frame fillBall
    else frameToResult frame

view : Model -> Html Msg
view model =
  table
    [ style "border" "2px solid black"
      , style "height" "150px"
      , style "margin" "10px"
    ]
    ( [ thead
           []
           ([th [style "text-align" "center", style "width" "56px", style "height" "36px"]
                [ text "Frame:" ]]
                ++
                generateHeader)
      ]
      ++
      [ tr
           [style  "border-style" "solid", style "border" "2px"]
           ([td [style "text-align" "center", style "width" "56px", style "height" "36px"]
                [ text "Result:" ]]
                ++
                (indexedMap (handleFrameList model.fillBall) (reverse model.frameList))
           )
      ]
      ++
      [ tr
           [style  "border-style" "solid", style "border" "2px"]
           ([td [style "text-align" "center", style "width" "56px", style "height" "36px"]
                [ text "Frame Score:" ]]
                ++
                [])
      ]
      ++
      [ tr
           [style  "border-style" "solid", style "border" "2px"]
           ([td [style "text-align" "center", style "width" "56px", style "height" "36px"]
                [ text "Total:" ]]
                ++
                [])
      ]
      ++
      [ tr
           []
           ([td [style "text-align" "center"
                 , colspan 2
                ]
                [ (input [ style "width" "60px", placeholder "Type here", value "", onInput Change ]
                         []) ],
             td [style "text-align" "center"]
                [ button [ onClick Reset ]
                         [ text "Reset" ] ]])
      ]
      ++
      [ tr
           [style  "border-style" "solid", style "border" "2px"]
           ([td [style "text-align" "center"
                 , colspan 11
                ]
                [ text "Type X for Strike, any digit in other case" ]])
     ]
    )

