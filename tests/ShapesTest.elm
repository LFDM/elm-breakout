module ShapesTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Shapes exposing (..)

rect x y w h = { x = x, y = y, w = w, h = h }

block = rect 10 30 40 -20 -- (10, 30) - (10, 10) - (50, 30) - (50, 10)

suite : Test
suite =
  describe "Shapes"
    [ describe "isTopHit"
      [ test "when outside left" <|
        \_ -> Expect.false "" <| isTopHit block (Line (Point 5 10) (Point 8 15))
      , test "when outside right" <|
        \_ -> Expect.false "" <| isTopHit block (Line (Point 55 10) (Point 45 8))
      , test "when outside above" <|
        \_ -> Expect.false "" <| isTopHit block (Line (Point 20 40) (Point 30 50))
      , test "when outside below" <|
        \_ -> Expect.false "" <| isTopHit block (Line (Point 20 0) (Point 15 5))
      , test "when hit from below" <|
        \_ -> Expect.false "" <| isTopHit block (Line (Point 8 8) (Point 11 11))
      , test "when hit from top I" <|
        \_ -> Expect.true "" <| isTopHit block (Line (Point 22 32) (Point 19 29))
      , test "when hit from top II" <|
        \_ -> Expect.true "" <| isTopHit (rect 730 780 80 20 ) (Line (Point 805 777) (Point 800 780))
      ]
    , describe "isBottomHit"
      [ test "when outside left" <|
        \_ -> Expect.false "" <| isBottomHit block (Line (Point 5 10) (Point 8 15))
      , test "when outside right" <|
        \_ -> Expect.false "" <| isBottomHit block (Line (Point 55 10) (Point 45 8))
      , test "when outside above" <|
        \_ -> Expect.false "" <| isBottomHit block (Line (Point 20 40) (Point 30 50))
      , test "when outside below" <|
        \_ -> Expect.false "" <| isBottomHit block (Line (Point 20 0) (Point 15 5))
      , test "when hit from below" <|
        \_ -> Expect.true "" <| isBottomHit block (Line (Point 8 8) (Point 11 11))
      , test "when moving from within the block outside" <|
        \_ -> Expect.false "" <| isBottomHit block (Line (Point 11 11) (Point 22 32))
      , test "when hit from top" <|
        \_ -> Expect.false "" <| isBottomHit block (Line (Point 22 32) (Point 19 29))
      ]

    , describe "haveIntersection"
      [ test "when they do" <|
        \_ -> Expect.true "" <| haveIntersection (Line (Point 0 0 ) (Point 10 10)) (Line (Point 0 10) (Point 10 0))
      , test "when they don't" <|
        \_ -> Expect.false "" <| haveIntersection (Line (Point 0 0 ) (Point 10 10)) (Line (Point 20 20) (Point 40 40))
      ]
    , describe "isWithin"
      [ test "detects inside" <|
        \_ -> Expect.true "" <| isWithin (rect 10 10 10 10) (Point 12 12)
      , test "detects outside" <|
        \_ -> Expect.false "" <| isWithin (rect 10 10 10 10) (Point 5 5)
      ]
    ]
