{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.GI.Base
import qualified GI.Gtk as Gtk

main :: IO ()
main = do
  Gtk.init Nothing
  win <- new Gtk.Window [#title := "Hi, there"]
  on win #destroy Gtk.mainQuit
  button <- new Gtk.Button [#label := "Click me"]
  on
    button
    #clicked
    ( set
        button
        [ #sensitive := False,
          #label := "Thanks for clicking me"
        ]
    )

  #add win button
  #showAll win
  Gtk.main
