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
      rightScore = 0
    }

-- | Window to draw our game.
window = createWindow width height offset "Pong"

-- | Background color of the window.
background :: Color
background = white

-- Our main function.
main :: IO ()
main = play window background fps initialState render' handleInput update
  where
    h = fromIntegral height
    w = fromIntegral width
    halfHeight = h / 2
    halfWidth = w / 2

    render' = render w h
    update = updatePong halfHeight (-halfHeight) (-halfWidth) halfWidth
    handleInput = handleUserInput halfHeight (-halfHeight)
