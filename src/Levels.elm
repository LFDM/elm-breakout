module Levels exposing (..)

import Types exposing (..)
import Color as C
import Array
import Dict

levels : Dict.Dict Int (List Block)
levels = Dict.fromList [
    (1, level1)
  ]

smBlock : Int -> Int -> C.Color -> Block
smBlock x y c = { x = x, y = y, w = 400, h = 200, color = c, acceleration = 1, score = 10 }

stdBlock : Int -> Int -> C.Color -> Block
stdBlock x y c = { x = x, y = y, w = 800, h = 300, color = c, acceleration = 1, score = 10 }

colorScale = Array.fromList [C.yellow, C.orange, C.red, C.green, C.blue, C.purple, C.lightYellow, C.lightOrange, C.lightRed, C.lightGreen, C.lightBlue]

getColor i =
  case Array.get i colorScale of
    Nothing -> C.black
    Just c -> c


level1 : List Block
level1 =
  let leftPad = 600
      leftMargin = 200
      topPad = 600
      topMargin = 400
      toColumn yI xI =
        let x = (leftPad + (800 * xI) + (leftMargin * xI))
            y = (topPad + (300 * yI) + (topMargin * yI))
        in stdBlock x y (getColor yI)
      toRow i = List.map (toColumn i) (List.range 0 6)
  in List.concatMap toRow (List.range 0 4)

level2 : List Block
level2 =
  let leftPad = 900
      leftMargin = 100
      topPad = 600
      topMargin = 300
      toColumn yI xI =
        let x = (leftPad + (400 * xI) + (leftMargin * xI))
            y = (topPad + (200 * yI) + (topMargin * yI))
        in smBlock x y (getColor xI)
      toRow i = List.map (toColumn i) (List.range 0 10)
  in List.concatMap toRow (List.range 0 7)

