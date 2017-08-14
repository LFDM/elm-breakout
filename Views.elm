module Views exposing (view)

import Types exposing (..)
import Shapes exposing (..)
import Html exposing (Html, text, div)
import Html.Attributes as HA
import Svg
import Svg.Attributes as SA

import Color as C

colorToCss : C.Color -> String
colorToCss color =
  let { red, green, blue, alpha } = C.toRgb color
      inner = (String.join "," << List.map toString) [red, green, blue]
  in "rgba(" ++ inner ++ "," ++ (toString alpha) ++ ")"


drawField : Rect a -> Svg.Svg Msg
drawField field =
  Svg.rect [
    SA.x <| toDim field.x,
    SA.y <| toDim field.y,
    SA.width <| toDim field.w,
    SA.height <| toDim field.h,
    SA.stroke "black",
    SA.fill "none"
  ] []

drawPaddle : Rect a -> Svg.Svg Msg
drawPaddle { x, y, w, h } =
  Svg.rect [
    SA.x <| toDim x,
    SA.y <| toDim y,
    SA.width <| toDim w,
    SA.height <| toDim h,
    SA.rx "5",
    SA.ry "5"
  ] []

drawBall : Int -> Int -> Svg.Svg Msg
drawBall ballX ballY =
  Svg.circle [
    SA.cx <| toDim ballX,
    SA.cy <| toDim ballY,
    SA.r "5"
  ] []

drawBlock : Block -> Svg.Svg Msg
drawBlock { x, y, w, h, color } =
  Svg.rect [
      SA.x <| toDim x,
      SA.y <| toDim y,
      SA.width <| toDim w,
      SA.height <| toDim h,
      SA.style <| "fill:" ++ (colorToCss color)
    ] []

drawBlocks : List Block -> List (Svg.Svg Msg)
drawBlocks = List.map drawBlock

drawGameHeader : Model -> Html Msg
drawGameHeader model =
  let score = text <| "Score: " ++ toString model.score
      s = HA.style [
        ("margin-left", (toDimPx model.field.x)),
        ("display", "flex"),
        ("justify-content", "space-between"),
        ("width", (toDimPx model.field.w))
      ]
  in case model.state of
    Won ->
        div [s]
          [ div [] [score], div [] [text "You won! Press space to start again"] ]
    Lost ->
      div [s]
        [ div [] [score], div [] [text "You lost! Press space to start again"] ]
    _ ->
      div [s] [score]

drawGameControls : Model -> Html Msg
drawGameControls model =
  div []
    []

drawGame : Model -> Html Msg
drawGame model =
  div []
    [ drawGameHeader model
    , Svg.svg [
        SA.width <| toDim (model.field.w + (model.field.x * 2)),
        SA.height <| toDim (model.field.h + (model.field.y * 2))
      ] ([
        drawField model.field,
        drawPaddle model.paddle,
        drawBall model.ball.x model.ball.y
      ] ++ drawBlocks model.blocks)
    , drawGameControls model
    ]

toDim : Int -> String
toDim value = (toString << round << (\x -> x / 10) << toFloat) value

toDimPx : Int -> String
toDimPx value = (toDim value) ++ "px"

view : Model -> Html Msg
view model =
  case model.state of
    Playing -> drawGame model
    Pause -> drawGame model
    Waiting -> text "Press SPACE to start"
    Lost -> drawGame model
    Won -> drawGame model
