module Main where

import qualified Graphics.UI.Gtk.OpenGL as GtkGL
import Control.Monad.IO.Class (liftIO)
import Graphics.UI.Gtk

import Program
import Rendering

main :: IO ()
main = do
  initGUI

  -- Initialize the GL tools and get a canvas.
  canvas <- initGL
  widgetSetSizeRequest canvas 640 480
  -- Create the GUI
  window <- windowNew
  set window [ windowTitle := "Gtk2Hs + HOpenGL demo"
             , containerChild := canvas
             ]

  -- Create the reactive network and get the plugs.
  plugs <- buildNetwork

  -- Event handlers
  canvas `on` realize $ onCanvasReady plugs canvas
  window `on` objectDestroy $ mainQuit
  timeoutAddFull (widgetQueueDraw canvas >> return True) priorityDefaultIdle animationWaitTime

  -- Start the application
  widgetShowAll window
  mainGUI

  where animationWaitTime = 3
