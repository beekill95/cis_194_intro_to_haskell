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
      pongPosition = (0, 0),
      pongVelocity = (75, 75)
    }

-- | Window to draw our game.
window = createWindow width height offset "Pong"

-- | Background color of the window.
background :: Color
background = white

-- Our main function.
main :: IO ()
main = simulate window background fps initialState render' update
  where
    h = fromIntegral height
    w = fromIntegral width
    halfHeight = h / 2

    render' = render w h
    update _ = updatePong halfHeight (-halfHeight)
