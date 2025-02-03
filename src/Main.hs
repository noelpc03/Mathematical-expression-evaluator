module Main where

import GUI (setup)
import Graphics.UI.Threepenny.Core (startGUI, defaultConfig)

main :: IO ()
main = startGUI defaultConfig setup