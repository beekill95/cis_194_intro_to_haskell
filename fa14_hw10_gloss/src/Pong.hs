module Pong where

import Graphics.Gloss

createWindow :: Int -> Int -> Int -> String -> Display
createWindow width height offset title = InWindow title (width, height) (offset, offset)

makePong :: Float -> Color -> Float -> Float -> Picture
makePong radius c x y = translate x y $ color c $ circleSolid radius

makePaddle :: Float -> Float -> Color -> Float -> Float -> Picture
makePaddle w h c x y = translate x y $ color c $ rectangleSolid w h

-- | Game state.
data PongGame = PongGame {
        -- | Position of the left paddle.
        leftPaddlePosition :: Float,

        -- | Position of the right paddle.
        rightPaddlePosition :: Float,

        -- | Location of the pong.
        pongPosition :: (Float, Float)
    }

-- | Initial state of the game.
initialState :: PongGame
initialState = PongGame {
        leftPaddlePosition = 0,
        rightPaddlePosition = 0,
        pongPosition = (0, 0)
    }

-- Render game state.
render :: Float -> Float -> PongGame -> Picture
render w h gameState = pictures [
        uncurry pong $ pongPosition gameState,
        leftPaddle $ leftPaddlePosition gameState,
        rightPaddle $ rightPaddlePosition gameState
    ]
    where
        -- Paddles.
        paddleColor = light $ light blue
        paddleWidth = 30
        paddleHeight = 150
        leftPaddle = makePaddle paddleWidth paddleHeight paddleColor (-w / 2)
        rightPaddle = makePaddle paddleWidth paddleHeight paddleColor (w / 2)

        -- Pong.
        pongColor = dark red
        pongRadius = 10
        pong = makePong pongRadius pongColor
