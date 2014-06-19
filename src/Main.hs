module Main (main) where

import Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk.OpenGL as GtkGL
import Graphics.Rendering.OpenGL as GL

import Control.Monad (forM, forM_)
import Control.Monad.Trans (liftIO)

main :: IO ()
main = do
  initGUI

  GtkGL.initGL
  glConfig <- GtkGL.glConfigNew [ GtkGL.GLModeRGBA
                                , GtkGL.GLModeDepth
                                , GtkGL.GLModeDouble
                                ]

  canvases@[top, front, side, perspective] <-
    mapM GtkGL.glDrawingAreaNew (replicate 4 glConfig)

  window <- windowNew
  window `on` deleteEvent $ liftIO mainQuit >> return False

  table <- tableNew 2 2 True
  tableSetRowSpacings table 5
  tableSetColSpacings table 5
  tableAttachDefaults table top         0 1 0 1
  tableAttachDefaults table front       0 1 1 2
  tableAttachDefaults table side        1 2 1 2
  tableAttachDefaults table perspective 1 2 0 1

  forM_ canvases $ \c -> do
    widgetSetSizeRequest c 200 200
    
    c `on` realize $ GtkGL.withGLDrawingArea c $ \_ -> do
      clearColor $= (Color4 0.3 0.3 0.3 0.3)
      matrixMode $= Projection
      loadIdentity
      ortho 0 1 0 1 (- 1) 1
      depthFunc $= Just Less
      drawBuffer $= BackBuffers

    c `on` exposeEvent $ do
      liftIO $ GtkGL.withGLDrawingArea c $ \glwindow -> do
        clear [DepthBuffer, ColorBuffer]
        cube 0.5
        GtkGL.glDrawableSwapBuffers glwindow
      return True

  set window [ containerChild := table
             , windowTitle := "Better Spline GUI"
             ]

  widgetShowAll window
  mainGUI

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z
  
cube :: GLfloat -> IO ()
cube w = do
  loadIdentity
  color (Color3 1 1 1 :: Color3 GLfloat)
  renderPrimitive Quads $ mapM_ vertex3f
    [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w)
    , ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w)
    , ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w)
    , (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w)
    , ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w)
    , ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w)
    ]
