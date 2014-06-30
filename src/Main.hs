module Main where

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.OpenGL as GtkGL
import Graphics.UI.Gtk (AttrOp((:=)), on)

import Program
import Rendering

main :: IO ()
main = do
  Gtk.initGUI

  -- Initialize the GL tools and get a canvas.
  canvas <- initGL
  Gtk.widgetSetSizeRequest canvas 640 480

  -- Create the GUI
  window <- Gtk.windowNew
  Gtk.onDestroy window Gtk.mainQuit
  Gtk.set window [ Gtk.windowTitle := "Gtk2Hs + HOpenGL demo"
                 , Gtk.containerChild := canvas
                 ]

  -- Create the reactive network and get the plugs.
  plugs <- buildNetwork canvas

  -- Fire the realize event when the canvas is ready. This creates the OpenGL resources
  -- and initializes rendering.
  Gtk.onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \_ -> (onRealize plugs) ()

  -- Make sure the redraw event is regularly fired.
  Gtk.timeoutAddFull (Gtk.widgetQueueDraw canvas >> return True) Gtk.priorityDefaultIdle animationWaitTime

  -- Start the application
  Gtk.widgetShowAll window
  Gtk.mainGUI

  where animationWaitTime = 3
