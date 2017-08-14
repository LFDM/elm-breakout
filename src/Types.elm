module Types exposing (..)

import Color as C
import Shapes exposing (..)

type alias Acceleration = Int

type alias Block = {
  score: Int,
  acceleration: Acceleration,
  x: Int,
  y: Int,
  w: Int,
  h: Int,
  color: C.Color
}

type alias VelocityChanger = ((Int, Int) -> (Int, Int))

type alias Moveable = {
  x: Int,
  y: Int,
  dx: Int,
  dy: Int
}

type State = Playing | Pause | Won | Lost | Waiting
type Msg = MouseMove Int Int | Tick | Space | P | Noop

type alias Model = {
  state: State,
  field: Rect {},
  paddle: Rect {},
  ball: Moveable,
  blocks : List Block,
  score : Int
}

type Hit = BlockHit Block | OtherHit

type alias HitEffect = {
  execute: VelocityChanger,
  hit: Hit,
  score: Int
}
