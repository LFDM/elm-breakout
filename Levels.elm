module Levels exposing (..)

import Types exposing (..)
import Color as C
import Array
import Dict

levels : Dict.Dict Int (List Block)
levels = Dict.fromList [
    (1, level1)
  ]

stdBlock : Int -> Int -> C.Color -> Block
stdBlock x y c = { x = x, y = y, w = 800, h = 300, color = c, acceleration = 1, score = 10 }

level1 : List Block
level1 =
  let leftPad = 600
      leftMargin = 200
      topPad = 600
      topMargin = 400
      colors = Array.fromList [C.yellow, C.orange, C.red, C.green, C.blue]
      getColor i =
        case Array.get i colors of
          Nothing -> C.black
          Just c -> c
      toColumn yI xI =
        let x = (leftPad + (800 * xI) + (leftMargin * xI))
            y = (topPad + (300 * yI) + (topMargin * yI))
        in stdBlock x y (getColor yI)
      toRow i = List.map (toColumn i) (List.range 0 6)
  in List.concatMap toRow (List.range 0 4)
