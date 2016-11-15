import Html.App as App
import Html exposing (Html, text)
import Mouse
import Svg
import Svg.Attributes as SA
import Time

type alias Block = (Int, Int)

type alias PlayingModel = {
    paddleX: Int,
    ballX: Int,
    ballY: Int,
    ballDX : Int,
    ballDY : Int,
    blocks : List Block
  }
type Model = Playing PlayingModel | Lost | Won
type Msg = MouseMove Int Int | UpdateBallPosition

startingBlocks : List Block
startingBlocks= [
    (15, 80), (66, 80), (117, 80), (168, 80), (219, 80), (270, 80), (321, 80), (372, 80), (423, 80), (474, 80), (525, 80),
    (555, 101), (504, 101), (453, 101), (402, 101), (351, 101), (300, 101), (249, 101), (198, 101), (147, 101), (96, 101), (45, 101),
    (15, 122), (66, 122), (117, 122), (168, 122), (219, 122), (270, 122), (321, 122), (372, 122), (423, 122), (474, 122), (525, 122)
  ]

initialState : Model
initialState = Playing {
    paddleX = 10,
    ballX = 20,
    ballY = 200,
    ballDX = 5,
    ballDY = 3,
    blocks = startingBlocks
  }

init : (Model, Cmd Msg)
init = (initialState, Cmd.none)

moveBall : PlayingModel -> PlayingModel
moveBall model =
  let { ballX, ballDX, ballY, ballDY } = model
      (newBallX, newBallY) = (ballX + ballDX, ballY + ballDY)
  in { model | ballX = newBallX, ballY = newBallY }

type CollisionType
  = PlayFieldTop
  | PlayFieldBottom
  | PlayFieldLeft
  | PlayFieldRight
  | PaddleTop
  | PaddleBottom
  | PaddleLeft
  | PaddleRight
  | BlockTop Block
  | BlockBottom Block
  | BlockLeft Block
  | BlockRight Block

detectTopWallCollision : PlayingModel -> Maybe CollisionType
detectTopWallCollision { ballY } =
  if ballY <= 13
    then Just PlayFieldTop
    else Nothing

detectBottomWallCollision : PlayingModel -> Maybe CollisionType
detectBottomWallCollision { ballY } =
  if ballY >= 607
    then Just PlayFieldBottom
    else Nothing

detectLeftWallCollision : PlayingModel -> Maybe CollisionType
detectLeftWallCollision { ballX } =
  if ballX <= 13
    then Just PlayFieldLeft
    else Nothing

detectRightWallCollision : PlayingModel -> Maybe CollisionType
detectRightWallCollision { ballX } =
  if ballX >= 607
    then Just PlayFieldRight
    else Nothing

collectWallCollisions : PlayingModel -> List CollisionType
collectWallCollisions model =
  List.filterMap identity [
      detectTopWallCollision model,
      detectBottomWallCollision model,
      detectLeftWallCollision model,
      detectRightWallCollision model
    ]

detectTopRectCollection : CollisionType -> Int -> Int -> Int -> Int -> PlayingModel -> Maybe CollisionType
detectTopRectCollection collision x y width height { ballX, ballY } =
  let left = x
      right = x + width
      bottom = y + height
  in
      if (ballX >= left - 2) && (ballX <= right + 2) && ballY >= y && (ballY <= bottom - 2)
        then Just collision
        else Nothing

detectBottomRectCollision : CollisionType -> Int -> Int -> Int -> Int -> PlayingModel -> Maybe CollisionType
detectBottomRectCollision collision x y width height { ballX, ballY } =
  let left = x
      right = x + width
      bottom = y + height
  in
      if (ballX >= left - 2) && (ballX <= right + 2) && (ballY >= y + 2) && (ballY <= bottom + 2)
        then Just collision
        else Nothing

detectLeftRectCollision : CollisionType -> Int -> Int -> Int -> Int -> PlayingModel -> Maybe CollisionType
detectLeftRectCollision collision x y width height { ballX, ballY } =
  let left = x
      right = x + width
      paddleTop = y
      bottom = y + height
  in
      if (ballX >= left - 2) && (ballX <= left + 2) && (ballY >= paddleTop - 1) && (ballY <= bottom + 1)
        then Just collision
        else Nothing

detectRightRectCollision : CollisionType -> Int -> Int -> Int -> Int -> PlayingModel -> Maybe CollisionType
detectRightRectCollision collision x y width height { ballX, ballY } =
  let left = x
      right = x + width
      paddleTop = y
      bottom = y + height
  in
      if (ballX >= right - 2) && (ballX <= right + 2) && (ballY >= paddleTop - 1) && (ballY <= bottom + 1)
        then Just collision
        else Nothing

collectPaddleCollisions : PlayingModel -> List CollisionType
collectPaddleCollisions model =
  if model.ballY < 570
    then []
    else List.filterMap identity [
          detectTopRectCollection PaddleTop model.paddleX 580 50 20 model,
          detectBottomRectCollision PaddleBottom model.paddleX 580 50 20 model,
          detectLeftRectCollision PaddleLeft model.paddleX 580 50 20 model,
          detectRightRectCollision PaddleRight model.paddleX 580 50 20 model
        ]

collectBlockCollisions : PlayingModel -> List CollisionType
collectBlockCollisions model =
  if model.ballY > 150
    then []
    else List.filterMap identity <| List.concatMap (\block ->
      let (x, y) = block
      in [
          detectTopRectCollection   (BlockTop block)    x y 50 20 model,
          detectBottomRectCollision (BlockBottom block) x y 50 20 model,
          detectLeftRectCollision   (BlockLeft block)   x y 50 20 model,
          detectRightRectCollision  (BlockRight block)  x y 50 20 model
        ]
      ) model.blocks

reflectVelocity : CollisionType -> (Int, Int) -> (Int, Int)
reflectVelocity collision (dx, dy) =
  case collision of
    PlayFieldTop -> (dx, -dy)
    PaddleTop    -> (dx, -dy)
    BlockTop _   -> (dx, -dy)

    PlayFieldBottom -> (dx, -dy)
    PaddleBottom    -> (dx, -dy)
    BlockBottom _   -> (dx, -dy)

    PlayFieldLeft -> (-dx, dy)
    PaddleLeft    -> (-dx, dy)
    BlockLeft _   -> (-dx, dy)

    PlayFieldRight -> (-dx, dy)
    PaddleRight    -> (-dx, dy)
    BlockRight _   -> (-dx, dy)

collectCollisions : PlayingModel -> List CollisionType
collectCollisions model = List.concat [
    collectWallCollisions model,
    collectPaddleCollisions model,
    collectBlockCollisions model
  ]

removeHitBlocks : CollisionType -> List Block -> List Block
removeHitBlocks collision blocks =
  let removeBlock blocks block = List.filter (\b -> b /= block) blocks
  in case collision of
    BlockTop block    -> removeBlock blocks block
    BlockBottom block -> removeBlock blocks block
    BlockLeft block   -> removeBlock blocks block
    BlockRight block  -> removeBlock blocks block
    _                 -> blocks

applyCollisions : PlayingModel -> List CollisionType -> PlayingModel
applyCollisions model collisions =
  let (newBallDX, newBallDY) = List.foldl reflectVelocity (model.ballDX, model.ballDY) collisions
      newBlocks = List.foldl removeHitBlocks model.blocks collisions
  in { model | ballDX = newBallDX, ballDY = newBallDY, blocks = newBlocks }

hasHitBottom : List CollisionType -> Bool
hasHitBottom collisions =
  List.any (\collision ->
    case collision of
      PlayFieldBottom -> True
      _ -> False
  ) collisions

handleCollisions : PlayingModel -> Model
handleCollisions model =
  let collisions = collectCollisions model
      postCollisions = applyCollisions model collisions
  in
    if hasHitBottom collisions
      then Lost
      else
        if (List.length postCollisions.blocks) > 0
          then Playing postCollisions
          else Won

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model of
    Lost -> (model, Cmd.none)
    Won  -> (model, Cmd.none)
    Playing model ->
      case msg of
        MouseMove mouseX _ ->
          let newPaddleX = clamp 10 560 mouseX
          in (Playing { model | paddleX = newPaddleX }, Cmd.none)
        UpdateBallPosition ->
          let newModel = handleCollisions <| moveBall model
          in (newModel, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  case model of
    Playing _ ->
      let moves = Mouse.moves (\{x, y} -> MouseMove x y)
          ticks = Time.every (Time.second / 40) <| always UpdateBallPosition
      in Sub.batch [ moves, ticks ]
    Lost -> Sub.none
    Won -> Sub.none

drawPlayingField : Svg.Svg Msg
drawPlayingField =
  Svg.rect [
    SA.x "10",
    SA.y "10",
    SA.width "600",
    SA.height "600",
    SA.stroke "black",
    SA.fill "none"
  ] []

drawPaddle : Int -> Svg.Svg Msg
drawPaddle paddleX =
  Svg.rect [
    SA.x <| toString paddleX,
    SA.y "580",
    SA.width "50",
    SA.height "20",
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
drawBlock (x, y) =
  Svg.rect [
      SA.x <| toString x,
      SA.y <| toString y,
      SA.width "50",
      SA.height "20"
    ] []

drawBlocks : List Block -> List (Svg.Svg Msg)
drawBlocks blocks =
  List.map drawBlock blocks

view : Model -> Html Msg
view model =
  case model of
    Playing model ->
      Svg.svg [
        SA.width "620",
        SA.height "620"
      ] ([
        drawPlayingField,
        drawPaddle model.paddleX,
        drawBall model.ballX model.ballY
      ] ++ drawBlocks model.blocks)
    Lost -> text "Game Over"
    Won -> text "Victory!"

main : Program Never
main = App.program {
    init = init,
    update = update,
    subscriptions = subscriptions,
    view = view
  }
