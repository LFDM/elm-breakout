module Shapes exposing (..)

type alias Rect a = { a |
  x: Int,
  y: Int,
  w: Int,
  h: Int
}

type alias Point = { x: Int, y: Int }

getRightX : Rect a -> Int
getRightX { x, w } = x + w

getTopRight : Rect a -> Point
getTopRight { x, y, w, h } = Point (x + w) (y + h)

getTopLeft : Rect a -> Point
getTopLeft { x, y, h } = Point x (y + h)

getBottomRight : Rect a -> Point
getBottomRight { x, w, y } = Point (x + w) y

getBottomLeft : Rect a -> Point
getBottomLeft { x, y } = Point x y


isLeftHit : Rect a -> Point -> Bool
isLeftHit r { x, y } =
  x >= r.x && x <= (r.x + r.w) && y >= (r.y + r.h) && y <= r.y

isRightHit : Rect a -> Point -> Bool
isRightHit r { x, y } =
  x <= (r.x + r.w) && x >= r.x && y >= (r.y + r.h) && y <= r.y

isXHit : Rect a -> Point -> Bool
isXHit r b = isLeftHit r b || isRightHit r b

isYHit : Rect a -> Point -> Bool
isYHit r b = isTopHit r b || isBottomHit r b

isTopHit : Rect a -> Point -> Bool
isTopHit r { x, y } =
  y >= r.y && y <= (r.y + r.h) && x >= r.x && x <= (r.x + r.w)

isBottomHit : Rect a -> Point -> Bool
isBottomHit r { x, y } =
  y <= (r.y + r.h) && y <= r.y && x >= r.x && x <= (r.x + r.w)
