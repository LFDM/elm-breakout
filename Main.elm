import Html exposing (Html, text, div)
import Html.Attributes as HA
import Keyboard
import Mouse
import Color as C
import Svg
import Svg.Attributes as SA
import Time
import Debug
import Array
import Shapes exposing (..)
import Types exposing (..)
import Levels

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

wallHit : VelocityChanger -> HitEffect
wallHit execute = { hit = OtherHit, score = 0, execute = execute }

paddleHit : VelocityChanger -> HitEffect
paddleHit execute = { hit = OtherHit, score = -1, execute = execute }

blockHit : (Acceleration -> VelocityChanger) -> Block -> HitEffect
blockHit fn block = { hit = BlockHit block, score = block.score, execute = fn (block.acceleration) }

initialState : Model
initialState = {
    state = Waiting,
    field = { x = 100, y = 100, w = 8000, h = 8000},
    paddle = { x = 3800, y = 7800, w = 1600, h = 200 },
    ball = { x = 4600, y = 7700, dx = 50, dy = -100 },
    blocks = Levels.level1,
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

rotateVector : (Int, Int) -> Int -> (Int, Int)
rotateVector (xI, yI) angle =
  let x = toFloat xI
      y = toFloat yI
      a = -(toFloat angle) * (pi / 180)
      c = cos a
      s = sin a
      nextX = round (x * c - y * s)
      nextY = round (x * s + y * c)
  in (nextX, nextY)

getAngle : (Int, Int) -> Int
getAngle (xI, yI) =
  let angle = atan2 (toFloat xI) (toFloat yI)
      degrees = (toFloat 180) * angle / pi
  in (360 + (round degrees)) % 360

paddleBounce : Rect a -> Moveable -> VelocityChanger
paddleBounce paddle ball =
  let angle = getAngle (ball.dx, ball.dy)
      paddleX1 = toFloat paddle.x
      paddleX2 = toFloat (paddle.x + paddle.w)
      segment = round (((toFloat ball.x) - paddleX1) / (paddleX2 - paddleX1) * 100 / 20)
      rotation =
        case segment of
          0 -> 180
          1 -> 180 - (angle * 2)
          2 -> 180 - (angle * 2)
          3 -> 180 - (angle * 2)
          4 -> 180
          _ -> 180 - (angle * 2)
      yyy = Debug.log "yyy" (rotation, segment, angle)
  in (\vector -> rotateVector vector rotation)

getPaddleHits : Model -> List (Maybe HitEffect) --there can be more than one
getPaddleHits { paddle, ball, score } =
  let top = if isTopHit paddle (asLine ball) then Just (paddleHit (paddleBounce paddle ball)) else Nothing
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
    else { model | state = Won, score = model.score + 500 }

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
      nextX = clamp leftBoundary rightBoundary (mouseX * 10)
      { paddle } = model
  in { model | paddle = { paddle | x = nextX } }

changeState : State -> Model -> (Model, Cmd Msg)
changeState state model =
  ({ model | state = state }, Cmd.none)

startPlay : (Model, Cmd Msg)
startPlay = changeState Playing initialState

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let noop = (model, Cmd.none)
  in case model.state of
    Pause ->
        case msg of
          Space -> changeState Playing model
          P -> changeState Playing model
          _ -> noop
    Playing ->
      case msg of
        MouseMove mouseX _ ->
          let nextModel = movePaddle model mouseX
          in (nextModel, Cmd.none)
        Tick ->
          let nextModel = handleHits <| moveBall model
          in (nextModel, Cmd.none)
        P -> changeState Pause model
        _ -> noop
    _ ->
      case msg of
        Space -> startPlay
        _ -> noop

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
    _ -> Keyboard.ups handleKeyUp

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

main : Program Never Model Msg
main = Html.program {
    init = init,
    update = update,
    subscriptions = subscriptions,
    view = view
  }
