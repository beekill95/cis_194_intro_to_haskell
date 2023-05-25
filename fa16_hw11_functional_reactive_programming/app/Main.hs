{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.GI.Base
import Data.Text
import qualified GI.Gtk as Gtk

main :: IO ()
main = coinUI

-- main = do
--   Gtk.init Nothing
--   win <- new Gtk.Window [#title := "Hi, there"]
--   on win #destroy Gtk.mainQuit
--   button <- new Gtk.Button [#label := "Click me"]
--   on
--     button
--     #clicked
--     ( set
--         button
--         [ #sensitive := False,
--           #label := "Thanks for clicking me"
--         ]
--     )

--   #add win button
--   #showAll win
--   Gtk.main

coinUI :: IO ()
coinUI = do
  _ <- Gtk.init Nothing

  win <- new Gtk.Window [#title := "Banana"]
  on win #destroy Gtk.mainQuit

  l <- new Gtk.ListBox []
  money <- new Gtk.Entry [#text := "5"]

  add <- new Gtk.Button [#label := "Insert Coin"]
  buy <- new Gtk.Button [#label := "Buy Banana", #sensitive := False]
  quit <- new Gtk.Button [#label := "Quit"]
  on quit #clicked Gtk.mainQuit

  on
    add
    #clicked
    ( do
        v <- read . unpack <$> get money #text
        let v' = v + 1 :: Integer
        set money [#text := (pack . show) v']
        when (v' >= 10) $ set buy [#sensitive := True]
    )

  on
    buy
    #clicked
    ( do
        v <- read . unpack <$> get money #text
        let v' = v - 10 :: Integer
        set money [#text := (pack . show) v']
        new
          Gtk.MessageDialog
          [ #title := "Yummy",
            #text := "You bought a banana!"
          ]
        return ()
    )

  #add l add
  #add l buy
  #add l money
  #add l quit
  #add win l

  #showAll win
  Gtk.main
