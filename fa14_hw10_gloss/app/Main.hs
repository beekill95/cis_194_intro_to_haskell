module Main where

import Graphics.Gloss
import Pong

-- | Window's width, height, and offset.
width, height, offset :: Int
width = 800
height = 500
offset = 200

window = createWindow width height offset "Pong"

background :: Color
background = white

drawing :: Picture
drawing = render (fromIntegral width) (fromIntegral height) initialState

main :: IO ()
main = display window background drawing
