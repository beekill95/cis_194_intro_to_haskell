module Pong where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char, SpecialKey), SpecialKey (KeyDown, KeyUp))

createWindow :: Int -> Int -> Int -> String -> Display
createWindow width height offset title =
  InWindow title (width, height) (offset, offset)

makePong :: Float -> Color -> Float -> Float -> Picture
makePong radius c x y = translate x y $ color c $ circleSolid radius

makePaddle :: Float -> Float -> Color -> Float -> Float -> Picture
makePaddle w h c x y = translate x y $ color c $ rectangleSolid w h

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
    pongVelocity :: (Float, Float)
  }

-- | Update pong position, taking into the top and bottom walls.
updatePong :: Float -> Float -> Float -> PongGame -> PongGame
updatePong top bottom timeElapsed = bouncePongOffWalls top bottom . movePong timeElapsed

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

-- Render game state.
render :: Float -> Float -> PongGame -> Picture
render w h gameState =
  pictures
    [ uncurry pong $ pongPosition gameState,
      leftPaddle $ leftPaddlePosition gameState,
      rightPaddle $ rightPaddlePosition gameState
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
    pongRadius = 10
    pong = makePong pongRadius pongColor
