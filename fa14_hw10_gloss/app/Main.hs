module Main where

import Graphics.Gloss
import Pong

-- | Window's width, height, and offset.
width, height, offset :: Int
width = 800
height = 500
offset = 200

-- | Frames per second.
fps :: Int
fps = 60

-- | Initial state of the game.
initialState :: PongGame
initialState =
  PongGame
    { leftPaddlePosition = 0,
      rightPaddlePosition = 0,
      paddleHeight = 150,
      paddleWidth = 30,
      pongRadius = 10,
      pongPosition = (0, 0),
      pongVelocity = (75, 75),
      leftScore = 0,
      rightScore = 0,
      topWall = fromIntegral height / 2,
      bottomWall = -(fromIntegral height / 2),
      leftWall = -(fromIntegral width / 2),
      rightWall = fromIntegral width / 2
    }

-- | Window to draw our game.
window = createWindow width height offset "Pong"

-- | Background color of the window.
background :: Color
background = white

-- | Reset the game when we have a winner.
resetGame :: Winner -> PongGame -> PongGame
resetGame winner game@(PongGame {leftScore = ls, rightScore = rs})
  | winner == LeftPlayer = game {leftScore = ls + 1, pongPosition = origin}
  | winner == RightPlayer = game {rightScore = rs + 1, pongPosition = origin}
  where
    origin = (0, 0)

-- Our main function.
main :: IO ()
main = play window background fps initialState render' handleInput update
  where
    -- FIXME: will be refactored to used the value inside game state.
    h = fromIntegral height
    w = fromIntegral width
    halfHeight = h / 2
    halfWidth = w / 2

    -- Render game state.
    render' = render w h

    -- Update state function.
    update t = updateScore' . updatePong' t
    updatePong' = updatePong halfHeight (-halfHeight) (-halfWidth) halfWidth
    updateScore' = updateScore resetGame

    -- Handle user's inputs.
    handleInput = handleUserInput halfHeight (-halfHeight)
