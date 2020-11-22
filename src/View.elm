module View exposing (..)

import Debug exposing (toString)
import Html.Attributes exposing (colspan, id, placeholder, value)
import Html.Events exposing (onClick, onInput)
import List exposing (indexedMap, map, reverse, take)
import Html exposing (Attribute, Html, button, input, table, td, text, th, thead, tr)
import List exposing (range)
import Model exposing (FillBalls, Frame(..), Model, fillBallToString, frameToString)
import String exposing (fromInt)
import Update exposing (Msg(..), computeScore, flattenModel, totalScore)

generateHeader: List (Html msg)
generateHeader = (range 1 10 |> map (\x -> th []  [ text (toString x) ]))

frameToResult: Frame -> Html Msg
frameToResult frame =
  td [ id "score-cell",  onClick DeleteLast ] [ text <| frameToString frame ]

lastFrameToResult: Frame -> FillBalls -> Html Msg
lastFrameToResult frame fillBalls =
  td [ id "score-cell", onClick DeleteLast ] [ text (frameToString frame ++ " " ++ fillBallToString fillBalls) ]

scoreToTableData: Int -> Html Msg
scoreToTableData score =
  td [ id "score-cell", onClick DeleteLast] [ text <| if score == 0 then "-" else fromInt score ]

handleFrameList: FillBalls -> Int -> Frame -> Html Msg
handleFrameList fillBalls i frame =
  if i == 9
    then lastFrameToResult frame fillBalls
    else frameToResult frame

view : Model -> Html Msg
view model =
  table
    []
    ( [ thead
           []
           ([th [ id "row-start" ]
                [ text "Frame:" ]]
                ++
                generateHeader)
      ]
      ++
      [ tr
           []
           ([td [ id "row-start" ]
                [ text "Result:" ]]
                ++
                (indexedMap (handleFrameList model.fillBalls) (reverse model.frameList))
           )
      ]
      ++
      [ tr
           []
           ([td [ id "row-start" ]
                [ text "Frame Score:" ]]
                ++
                (map scoreToTableData (take 10 <| computeScore (flattenModel (reverse model.frameList) model.fillBalls))))
      ]
      ++
      [ tr
           []
           ([td [ id "row-start" ]
                [ text "Total:" ]]
                ++
                (map scoreToTableData (take 10 <| totalScore <| computeScore (flattenModel (reverse model.frameList) model.fillBalls))))
      ]
      ++
      [ tr
           []
           ([td [ colspan 2 ]
                [ (input [ placeholder "Type here", value "", onInput Change ]
                         []) ],
             td []
                [ button [ onClick Reset ]
                         [ text "Reset" ] ]])
      ]
      ++
      [ tr
           []
           ([td [ colspan 11 ]
                [ text "Type \"x\" for a Strike or type a digit for the number of pins hit" ]])
      ]
      ++
      [ tr
           []
           ([td [ colspan 11 ]
                [ text "Click any frame result to delete the last frame" ]])
      ]
    )

