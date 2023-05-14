module Pong where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char, SpecialKey), SpecialKey (KeyDown, KeyUp))

-- | Game state.
data PongGame = PongGame
  { -- | Position of the left paddle.
    leftPaddlePosition :: Float,
    -- | Position of the right paddle.
    rightPaddlePosition :: Float,
    -- | Height of the paddles.
    paddleHeight :: Float,
    -- | Width of the paddles.
    paddleWidth :: Float,
    -- | Location of the pong.
    pongPosition :: (Float, Float),
    -- | Velocity of the pong.
    pongVelocity :: (Float, Float),
    -- | Radius of the pong.
    pongRadius :: Float,
    -- | Left player's score.
    leftScore :: Int,
    -- | Right player's score.
    rightScore :: Int
  }

-- | Update pong position, taking into the top and bottom walls.
updatePong ::
  -- | Position of the top wall.
  Float ->
  -- | Position of the bottom wall.
  Float ->
  -- | Position of the left wall.
  Float ->
  -- | Position of the right wall.
  Float ->
  -- | Time elapsed since the last update.
  Float ->
  -- | Current game state.
  PongGame ->
  -- | The updated game state.
  PongGame
updatePong top bottom left right timeElapsed =
  bouncePongOffPaddles left right
    . bouncePongOffWalls top bottom
    . movePong timeElapsed

-- | Move the pong based on current velocity and time elapsed.
movePong :: Float -> PongGame -> PongGame
movePong timeElapsed state = state {pongPosition = (newX, newY)}
  where
    (x, y) = pongPosition state
    (vx, vy) = pongVelocity state

    newX = x + vx * timeElapsed
    newY = y + vy * timeElapsed

-- | Bounce the pong off top and bottom walls.
bouncePongOffWalls :: Float -> Float -> PongGame -> PongGame
bouncePongOffWalls topWall bottomWall state
  | y >= topWall = state {pongVelocity = (vx, -vy)}
  | y <= bottomWall = state {pongVelocity = (vx, -vy)}
  | otherwise = state
  where
    (_, y) = pongPosition state
    (vx, vy) = pongVelocity state

-- | Bounce the pong off the paddles.
bouncePongOffPaddles :: Float -> Float -> PongGame -> PongGame
bouncePongOffPaddles leftWall rightWall state
  | hitLeftPaddle (poX - radius, poY)
      || hitRightPaddle (poX + radius, poY) =
      state {pongVelocity = (-vx, vy)}
  | otherwise = state
  where
    radius = pongRadius state
    (poX, poY) = pongPosition state
    (vx, vy) = pongVelocity state

    paddleSize = (paddleHeight state, paddleWidth state)
    hitLeftPaddle = hitPaddle (leftWall, leftPaddlePosition state) paddleSize
    hitRightPaddle = hitPaddle (rightWall, rightPaddlePosition state) paddleSize

-- | Check if a pong is in contact with a paddle.
hitPaddle ::
  -- | (x, y) position of the paddle.
  (Float, Float) ->
  -- | (h, w) height and width of the paddle.
  (Float, Float) ->
  -- | (x, y) of the pong.
  (Float, Float) ->
  Bool
hitPaddle (pX, pY) (pH, pW) (poX, poY) =
  isIn (pX - halfWidth) (pX + halfWidth) poX
    && isIn (pY - halfHeight) (pY + halfHeight) poY
  where
    halfWidth = pW / 2
    halfHeight = pH / 2

    isIn lower upper x = (lower <= x) && (x <= upper)

-- | Handle user inputs to move the paddles.
-- w and s to move the left paddle up and down, respectively.
-- Arrow ↑ and ↓ to move the right paddle up and down, respectively.
handleUserInput :: Float -> Float -> Event -> PongGame -> PongGame
handleUserInput topWall bottomWall (EventKey c _ _ _) state =
  case c of
    (Char 'w') -> state {leftPaddlePosition = paddleUp' leftPaddle}
    (Char 's') -> state {leftPaddlePosition = paddleDown' leftPaddle}
    (SpecialKey KeyUp) -> state {rightPaddlePosition = paddleUp' rightPaddle}
    (SpecialKey KeyDown) -> state {rightPaddlePosition = paddleDown' rightPaddle}
    _ -> state
  where
    moveDistance = 20
    paddleUp' = paddleUp moveDistance topWall (paddleHeight state)
    paddleDown' = paddleDown moveDistance bottomWall (paddleHeight state)

    leftPaddle = leftPaddlePosition state
    rightPaddle = rightPaddlePosition state
handleUserInput _ _ _ state = state

-- | Move the paddle up.
paddleUp ::
  -- | How far should the paddle move up.
  Float ->
  -- | The position of the top wall.
  Float ->
  -- | The height of the paddle.
  Float ->
  -- | The position of the paddle.
  Float ->
  -- | The position of the paddle after moving.
  Float
paddleUp inc topWall paddleHeight paddle =
  min (paddle + inc) (topWall - halfHeight)
  where
    halfHeight = paddleHeight / 2

-- | Move the paddle down.
paddleDown ::
  -- | How far should the paddle move down.
  Float ->
  -- | The position of the bottom wall.
  Float ->
  -- | The height of the paddle.
  Float ->
  -- | The position of the paddle.
  Float ->
  -- | The position of the paddle after moving.
  Float
paddleDown dec bottomWall paddleHeight paddle =
  max (paddle - dec) (bottomWall + halfHeight)
  where
    halfHeight = paddleHeight / 2

-- Rendering.
createWindow :: Int -> Int -> Int -> String -> Display
createWindow width height offset title =
  InWindow title (width, height) (offset, offset)

makePong :: Float -> Color -> Float -> Float -> Picture
makePong radius c x y = translate x y $ color c $ circleSolid radius

makePaddle :: Float -> Float -> Color -> Float -> Float -> Picture
makePaddle w h c x y = translate x y $ color c $ rectangleSolid w h

makeScores :: Float -> Float -> Color -> Int -> Int -> Picture
makeScores x y c leftScore rightScore =
  translate x y $
    color c $
      text $
        show leftScore ++ "/" ++ show rightScore

-- Render game state.
render :: Float -> Float -> PongGame -> Picture
render w h gameState =
  pictures
    [ uncurry pong $ pongPosition gameState,
      leftPaddle $ leftPaddlePosition gameState,
      rightPaddle $ rightPaddlePosition gameState,
      score (leftScore gameState) (rightScore gameState)
    ]
  where
    -- Paddles.
    paddleColor = light $ light blue
    pw = paddleWidth gameState
    ph = paddleHeight gameState
    leftPaddle = makePaddle pw ph paddleColor (-w / 2)
    rightPaddle = makePaddle pw ph paddleColor (w / 2)

    -- Pong.
    pongColor = dark red
    radius = pongRadius gameState
    pong = makePong radius pongColor

    -- Players' score.
    scoreColor = black
    score = makeScores 0 (w / 2) scoreColor
