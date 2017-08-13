import Html exposing (Html, text)
import Keyboard
import Mouse
import Color as C
import Svg
import Svg.Attributes as SA
import Time

type alias Point = {
  x: Int,
  y: Int
}

type alias Rect a = { a |
  x: Int,
  y: Int,
  w: Int,
  h: Int
}

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

type alias VelocityChanger = (Int, Int) -> (Int, Int)

type alias Moveable = {
  x: Int,
  y: Int,
  dx: Int,
  dy: Int
}

type State = Playing | Won | Lost | Waiting
type Msg = MouseMove Int Int | Tick | Space | Noop

type alias Model = {
  state: State,
  field: Rect {},
  paddle: Rect {},
  ball: Moveable,
  blocks : List Block
}

type Hit = BlockHit Block | OtherHit

type alias HitEffect = {
  execute: VelocityChanger,
  hit: Hit,
  score: Int
}

getRightX : Rect a -> Int
getRightX { x, w } = x + w

colorToCss : C.Color -> String
colorToCss color =
  let { red, green, blue, alpha } = C.toRgb color
      inner = (String.join "," << List.map toString) [red, green, blue]
  in "rgba(" ++ inner ++ "," ++ (toString alpha) ++ ")"

stdBlock : Int -> Int -> C.Color -> Block
stdBlock x y c = { x = x, y = y, w = 50, h = 20, color = c, acceleration = 1, score = 10 }

level1 : List Block
level1 = [
    stdBlock 10 80 C.red,
    stdBlock 66 80 C.red,
    stdBlock 117 80 C.red
  ]

initialState : Model
initialState = {
    state = Waiting,
    field = { x = 10, y = 10, w = 600, h = 600 },
    paddle = { x = 20, y = 580, w = 80, h = 20 },
    ball = { x = 20, y = 200, dx = 5, dy = 3 },
    blocks = level1
  }

init : (Model, Cmd Msg)
init = (initialState, Cmd.none)

moveBall : Model -> Model
moveBall model =
  let { ball } = model
      nextBall = { ball | x = ball.x + ball.dx, y = ball.y + ball.dy }
  in { model | ball = nextBall  }

applyHits : Model -> List (Maybe HitEffect) -> Model
applyHits model hits = model

collectHits : Model -> List (Maybe HitEffect)
collectHits model = []

hasHitBottom : Rect a -> Moveable -> Bool
hasHitBottom field ball = (field.y + field.h) <= ball.y

checkForWin : Model -> Model
checkForWin model =
  if (List.length model.blocks) > 0
    then model
    else { model | state = Won }

handleHits : Model -> Model
handleHits model =
  if hasHitBottom model.field model.ball
    --then { model | state = Lost }
    then model
    else
      (checkForWin << (applyHits model) << collectHits) model


movePaddle : Model -> Int -> Model
movePaddle model mouseX =
  let leftBoundary = model.field.x
      rightBoundary = (getRightX model.field) - model.paddle.w
      nextX = clamp leftBoundary rightBoundary mouseX
      { paddle } = model
  in { model | paddle = { paddle | x = nextX } }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model.state of
    Lost -> (model, Cmd.none)
    Won  -> (model, Cmd.none)
    Waiting ->
      case msg of
        Space ->
          let nextModel = { model | state = Playing }
          in (nextModel, Cmd.none)
        _ -> (model, Cmd.none)

    Playing ->
      case msg of
        MouseMove mouseX _ ->
          let nextModel = movePaddle model mouseX
          in (nextModel, Cmd.none)
        Tick ->
          let nextModel = handleHits <| moveBall model
          in (nextModel, Cmd.none)
        _ -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.state of
    Playing ->
      let moves = Mouse.moves (\{x, y} -> MouseMove x y)
          ticks = Time.every (Time.second / 40) <| always Tick
      in Sub.batch [ moves, ticks ]
    Waiting -> Keyboard.ups (\kc -> if kc == 32 then Space else Noop)

    Lost -> Sub.none
    Won -> Sub.none

drawField : Rect a -> Svg.Svg Msg
drawField field =
  Svg.rect [
    SA.x <| toString field.x,
    SA.y <| toString field.y,
    SA.width <| toString field.w,
    SA.height <| toString field.h,
    SA.stroke "black",
    SA.fill "none"
  ] []

drawPaddle : Rect a -> Svg.Svg Msg
drawPaddle { x, y, w, h } =
  Svg.rect [
    SA.x <| toString x,
    SA.y <| toString y,
    SA.width <| toString w,
    SA.height <| toString h,
    SA.rx "5",
    SA.ry "5"
  ] []

drawBall : Int -> Int -> Svg.Svg Msg
drawBall ballX ballY =
  Svg.circle [
    SA.cx <| toString ballX,
    SA.cy <| toString ballY,
    SA.r "5"
  ] []

drawBlock : Block -> Svg.Svg Msg
drawBlock { x, y, w, h, color } =
  Svg.rect [
      SA.x <| toString x,
      SA.y <| toString y,
      SA.width <| toString w,
      SA.height <| toString h,
      SA.style <| "fill:" ++ (colorToCss color)
    ] []

drawBlocks : List Block -> List (Svg.Svg Msg)
drawBlocks = List.map drawBlock

view : Model -> Html Msg
view model =
  case model.state of
    Playing ->
      Svg.svg [
        SA.width <| toString (model.field.w + (model.field.x * 2)),
        SA.height <| toString (model.field.h + (model.field.y * 2))
      ] ([
        drawField model.field,
        drawPaddle model.paddle,
        drawBall model.ball.x model.ball.y
      ] ++ drawBlocks model.blocks)
    Waiting -> text "Press SPACE to start"
    Lost -> text "You lost"
    Won -> text "You won"

main : Program Never Model Msg
main = Html.program {
    init = init,
    update = update,
    subscriptions = subscriptions,
    view = view
  }
