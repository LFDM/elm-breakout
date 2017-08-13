module Shapes exposing (..)
import Debug

type alias Rect a = { a |
  x: Int,
  y: Int,
  w: Int,
  h: Int
}

type alias Point = { x: Int, y: Int }

type alias Line = { a: Point, b: Point }

getTopRight : Rect a -> Point
getTopRight { x, y, w } = Point (x + w) (y)

getTopLeft : Rect a -> Point
getTopLeft { x, y } = Point x y

getBottomRight : Rect a -> Point
getBottomRight { x, y, w, h } = Point (x + w) (y + h)

getBottomLeft : Rect a -> Point
getBottomLeft { x, y, h } = Point x (y + h)

isLeftHit : Rect a -> Line -> Bool
isLeftHit r trajectory =
  let wall = Line (getTopLeft r) (getBottomLeft r)
  in not (isWithin r trajectory.a) && haveIntersection wall trajectory

isRightHit : Rect a -> Line -> Bool
isRightHit r trajectory =
  let wall = Line (getTopRight r) (getBottomRight r)
  in not (isWithin r trajectory.a) && haveIntersection wall trajectory

isTopHit : Rect a -> Line -> Bool
isTopHit r trajectory =
  let wall = Line (getTopLeft r) (getTopRight r)
  in not (isWithin r trajectory.a) && haveIntersection wall trajectory

isBottomHit : Rect a -> Line -> Bool
isBottomHit r trajectory =
  let wall = Line (getBottomLeft r) (getBottomRight  r)
  in not (isWithin r trajectory.a) && haveIntersection wall trajectory

isXHit : Rect a -> Line -> Bool
isXHit r b = isLeftHit r b || isRightHit r b

isYHit : Rect a -> Line -> Bool
isYHit r b = isTopHit r b || isBottomHit r b

isWithin : Rect a -> Point -> Bool
isWithin r { x, y } =
  let a = getTopLeft r
      d = getBottomRight r
  in x >= a.x && x <= d.x && y >= a.y && y <= d.y


haveIntersection : Line -> Line -> Bool
haveIntersection { a, b } l =
  let c = l.a
      d = l.b
      denom = ((d.y - c.y) * (b.x - a.x)) - ((d.x - c.x) * (b.y - a.y))
      numeA = ((d.x - c.x) * (a.y - c.y)) - ((d.y - c.y) * (a.x - c.x))
      numeB = ((b.x - a.x) * (a.y - c.y)) - ((b.y - a.y) * (a.x - c.x))

      uA = (toFloat numeA) / (toFloat denom)
      uB = (toFloat numeB) / (toFloat denom)
  in if denom == 0
      then False
      else uA >= 0 && uA <= 1 && uB >= 0 && uB <= 1
