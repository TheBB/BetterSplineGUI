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
  cp <- initGL
  widgetSetSizeRequest cp 320 240

  -- Create the GUI
  window <- windowNew
  table <- tableNew 1 2 False
  tableAttachDefaults table cp 0 1 0 1
  tableSetRowSpacings table 10
  tableSetColSpacings table 10

  set window [ windowTitle := "Gtk2Hs + HOpenGL demo"
             , containerChild := table
             ]

  -- Create the reactive network and get the plugs.
  plugs <- buildNetwork table

  -- Event handlers
  cp `on` realize $ onCanvasRealize plugs cp >> onMasterRealize plugs cp
  window `on` objectDestroy $ mainQuit
  timeoutAddFull (widgetQueueDraw cp >> return True) priorityDefaultIdle animationWaitTime

  -- Start the application
  widgetShowAll window
  mainGUI

  where animationWaitTime = 3
