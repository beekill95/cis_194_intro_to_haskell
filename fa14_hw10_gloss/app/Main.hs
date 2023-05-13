module Main where

import Graphics.Gloss

window :: Display
window = InWindow "Hello, world!" (200, 200) (200, 200)

background :: Color
background = white

drawing :: Picture
drawing = circle 80

main :: IO ()
main = display window background drawing