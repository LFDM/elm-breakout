import Html exposing (Html, text)
import Keyboard
import Mouse
import Color as C
import Svg
import Svg.Attributes as SA
import Time
import Debug
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

asLine : Moveable -> Line
asLine { x, y, dx, dy } = Line (Point (x - dx) (y - dy)) (Point x y)

bounceX : Acceleration -> VelocityChanger
bounceX acc = (\(dx, dy) -> (-dx * acc, dy * acc))

bounceY : Acceleration -> VelocityChanger
bounceY acc = (\(dx, dy) -> (dx * acc, -dy * acc))

colorToCss : C.Color -> String
colorToCss color =
  let { red, green, blue, alpha } = C.toRgb color
      inner = (String.join "," << List.map toString) [red, green, blue]
  in "rgba(" ++ inner ++ "," ++ (toString alpha) ++ ")"

stdBlock : Int -> Int -> C.Color -> Block
stdBlock x y c = { x = x, y = y, w = 90, h = 30, color = c, acceleration = 1, score = 10 }

wallHit : VelocityChanger -> HitEffect
wallHit execute = { hit = OtherHit, score = 0, execute = execute }

paddleHit : VelocityChanger -> HitEffect
paddleHit execute = { hit = OtherHit, score = -1, execute = execute }

blockHit : (Acceleration -> VelocityChanger) -> Block -> HitEffect
blockHit fn block = { hit = BlockHit block, score = block.score, execute = fn (block.acceleration) }

level1 : List Block
level1 = [
    stdBlock 20 80 C.red,
    stdBlock 120 80 C.red,
    stdBlock 220 80 C.red,
    stdBlock 120 180 C.red,
    stdBlock 220 180 C.red,
    stdBlock 20 180 C.red,
    stdBlock 120 180 C.red
  ]

initialState : Model
initialState = {
    state = Waiting,
    field = { x = 10, y = 10, w = 800, h = 800},
    paddle = { x = 20, y = 780, w = 80, h = 20 },
    ball = { x = 20, y = 250, dx = 5, dy = 3 },
    blocks = level1,
    score = 0
  }

init : (Model, Cmd Msg)
init = (initialState, Cmd.none)

moveBall : Model -> Model
moveBall model =
  let { ball } = model
      nextBall = { ball | x = ball.x + ball.dx, y = ball.y + ball.dy }
  in { model | ball = nextBall  }

applyVelocity : VelocityChanger -> Moveable -> Moveable
applyVelocity v m =
  let (dx, dy) = v (m.dx, m.dy)
  in { m | dx = dx, dy = dy }

applyHit : (Maybe HitEffect) -> Model -> Model
applyHit hE m =
  case hE of
    Nothing -> m
    Just h ->
      case h.hit of
        OtherHit -> { m | score = m.score + h.score, ball = applyVelocity h.execute m.ball }
        BlockHit block -> { m |
            score = m.score + h.score,
            ball = applyVelocity h.execute m.ball,
            blocks = List.filter (\b -> b /= block) m.blocks
          }

applyHits : Model -> List (Maybe HitEffect) -> Model
applyHits = List.foldl applyHit

getXWallHit : Model -> Maybe HitEffect
getXWallHit { field, ball } =
  let hitRight { x, w } b = x + w <= b.x
      hitLeft { x } b = x >= b.x
  in if hitRight field ball || hitLeft field ball
    then Just (wallHit (bounceX 1))
    else Nothing

getUpperWallHit : Model -> Maybe HitEffect
getUpperWallHit { field, ball } =
  if hasHitTop field ball
    then Just (wallHit (bounceY 1))
    else Nothing

getPaddleHits : Model -> List (Maybe HitEffect) --there can be more than one
getPaddleHits { paddle, ball, score } =
  let x = Debug.log "hit" (paddle, (isTopHit paddle (asLine ball)), asLine ball, getTopRight paddle, getTopLeft paddle)
      top = if isTopHit paddle (asLine ball) then Just (paddleHit (bounceY 1)) else Nothing
  in [top]

getBlockHit : Moveable -> Block -> List (Maybe HitEffect)
getBlockHit ball block =
  let y = if isYHit block (asLine ball) then Just (blockHit bounceY block) else Nothing
      x = if isXHit block (asLine ball) then Just (blockHit bounceX block) else Nothing
  in [y, x]

getBlockHits : Model -> List (Maybe HitEffect)
getBlockHits { blocks, ball } = List.concatMap (getBlockHit ball) blocks

collectWallHits : Model -> List (Maybe HitEffect)
collectWallHits model = [ getXWallHit model, getUpperWallHit model ]

collectHits : Model -> List (Maybe HitEffect)
collectHits model = List.concat [
    collectWallHits model,
    getPaddleHits model,
    getBlockHits model
  ]

hasHitBottom : Rect a -> Moveable -> Bool
hasHitBottom field ball = (field.y + field.h) <= ball.y

hasHitTop : Rect a -> Moveable -> Bool
hasHitTop field ball = field.y >= ball.y

checkForWin : Model -> Model
checkForWin model =
  if (List.length model.blocks) > 0
    then model
    else { model | state = Won }

handleHits : Model -> Model
handleHits model =
  if hasHitBottom model.field model.ball
    then { model | state = Lost }
    else
      (checkForWin << (applyHits model) << collectHits) model

movePaddle : Model -> Int -> Model
movePaddle model mouseX =
  let leftBoundary = model.field.x
      rightBoundary = (.x (getTopRight model.field)) - model.paddle.w
      nextX = clamp leftBoundary rightBoundary mouseX
      { paddle } = model
  in { model | paddle = { paddle | x = nextX } }

changeState : State -> Model -> (Model, Cmd Msg)
changeState state model =
  ({ model | state = state }, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model.state of
    Lost -> (model, Cmd.none)
    Won  -> (model, Cmd.none)
    Waiting ->
      case msg of
        Space -> changeState Playing model
        _ -> (model, Cmd.none)

    Pause ->
        case msg of
          Space -> changeState Playing model
          P -> changeState Playing model
          _ -> (model, Cmd.none)
    Playing ->
      case msg of
        MouseMove mouseX _ ->
          let nextModel = movePaddle model mouseX
          in (nextModel, Cmd.none)
        Tick ->
          let nextModel = handleHits <| moveBall model
          in (nextModel, Cmd.none)
        P -> changeState Pause model
        _ -> (model, Cmd.none)

handleKeyUp : Int -> Msg
handleKeyUp kc =
  case kc of
    32 -> Space
    80 -> P
    _ -> Noop

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.state of
    Playing ->
      let moves = Mouse.moves (\{x, y} -> MouseMove x y)
          keys = Keyboard.ups handleKeyUp
          ticks = Time.every (Time.second / 40) <| always Tick
      in Sub.batch [ moves, keys, ticks ]
    Pause -> Keyboard.ups handleKeyUp
    Waiting -> Keyboard.ups handleKeyUp

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

drawGame : Model -> Html Msg
drawGame model =
  Svg.svg [
    SA.width <| toString (model.field.w + (model.field.x * 2)),
    SA.height <| toString (model.field.h + (model.field.y * 2))
  ] ([
    drawField model.field,
    drawPaddle model.paddle,
    drawBall model.ball.x model.ball.y
  ] ++ drawBlocks model.blocks)

view : Model -> Html Msg
view model =
  case model.state of
    Playing -> drawGame model
    Pause -> drawGame model
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
