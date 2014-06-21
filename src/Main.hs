{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as VC
import qualified Foreign.Storable as F
import Control.Applicative
import Control.Monad (unless)
import Data.FileEmbed (embedFile)
import System.Clock
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO

import qualified Data.Vinyl as V
import qualified Graphics.GLUtil as U
import qualified Graphics.GLUtil.Camera3D as U
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.OpenGL as GtkGL
import qualified Graphics.VinylGL as VG
import qualified Linear as L
import Data.Vinyl ((:::), (=:), (<+>))
import Linear ((!*!))
import Graphics.Rendering.OpenGL (($=))
import Graphics.UI.Gtk (AttrOp((:=)), on)

import Control.Event.Handler
import Reactive.Banana
import Reactive.Banana.Frameworks

main :: IO ()
main = do
  GLFW.init
  Gtk.initGUI
  GtkGL.initGL
  
  glconfig <- GtkGL.glConfigNew [ GtkGL.GLModeRGBA
                                , GtkGL.GLModeDepth
                                , GtkGL.GLModeDouble
                                ]
  canvas <- GtkGL.glDrawingAreaNew glconfig
  Gtk.widgetSetSizeRequest canvas 640 480

  (addOnRealize, onRealize) <- newAddHandler

  network <- compile (makeNetwork addOnRealize canvas)
  actuate network

  Gtk.onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \_ -> onRealize ()
  Gtk.timeoutAddFull (Gtk.widgetQueueDraw canvas >> return True) Gtk.priorityDefaultIdle animationWaitTime

  window <- Gtk.windowNew
  Gtk.onDestroy window Gtk.mainQuit
  Gtk.set window [ Gtk.windowTitle := "Gtk2Hs + HOpenGL demo"
                 , Gtk.containerChild := canvas
                 ]

  Gtk.widgetShowAll window
  Gtk.mainGUI

  -- Gtk.onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \_ -> do
  --   program <- initResources
  --   Gtk.onExpose canvas $ \_ -> do
  --     GtkGL.withGLDrawingArea canvas $ \glwindow -> do
  --       draw program glwindow
  --       GtkGL.glDrawableSwapBuffers glwindow
  --     return True
  --   return ()

  -- Gtk.timeoutAddFull (Gtk.widgetQueueDraw canvas >> return True) Gtk.priorityDefaultIdle animationWaitTime
    
  -- window <- Gtk.windowNew
  -- Gtk.onDestroy window Gtk.mainQuit
  -- Gtk.set window [ Gtk.windowTitle := "Gtk2Hs + HOpenGL demo"
  --                , Gtk.containerChild := canvas
  --                ]

  -- Gtk.widgetShowAll window
  -- Gtk.mainGUI

makeNetwork :: Frameworks t => AddHandler () -> GtkGL.GLDrawingArea -> Moment t ()
makeNetwork addOnRealize canvas = do
  eRealize <- fromAddHandler addOnRealize
  reactimate $ fmap doRealize eRealize
  where doRealize _ = do
          program <- initResources
          Gtk.onExpose canvas $ \_ -> do
            GtkGL.withGLDrawingArea canvas $ \glwindow -> do
              draw program glwindow
              GtkGL.glDrawableSwapBuffers glwindow
            return True
          return ()

vSrc = $(embedFile "src/shader/vs.glsl") :: BS.ByteString
fSrc = $(embedFile "src/shader/fs.glsl") :: BS.ByteString

initResources :: IO Resources
initResources = do
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.drawBuffer $= GL.BackBuffers

  Resources
    <$> U.simpleShaderProgramBS vSrc fSrc
    <*> VG.bufferVertices (zipWith (<+>) vertices colors)
    <*> U.fromSource GL.ElementArrayBuffer elements

draw :: Resources -> GtkGL.GLWindow -> IO ()
draw (Resources s vb e) glwindow = do
  GL.clearColor $= (GL.Color4 1 1 1 1)
  GL.depthFunc $= Just GL.Less
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  
  (width, height) <- GtkGL.glDrawableGetSize glwindow
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
  
  t <- maybe 0 id <$> GLFW.getTime

  GL.currentProgram $= (Just . U.program $ s)
  VG.enableVertices' s vb
  VG.bindVertices vb
  VG.setAllUniforms s $ transformM width height t
  GL.bindBuffer GL.ElementArrayBuffer $= Just e
  U.drawIndexedTris (fromIntegral $ length elements)

transformM :: Int -> Int -> Double -> V.PlainRec '[MVP]
transformM width height t = mvp =: (proj !*! view !*! model !*! anim)
  where anim  = L.mkTransformation (L.axisAngle (L.V3 0 1 0) angle) L.zero
        model = L.mkTransformationMat L.eye3 $ L.V3 0 0 (-4)
        view  = U.camMatrix $ U.tilt (-30) . U.dolly (L.V3 0 2 0) $ U.fpsCamera
        proj  = U.projectionMatrix (pi/4) aspect 0.1 10

        angle = realToFrac t * pi / 4
        aspect = fromIntegral width / fromIntegral height

vertices :: [V.PlainRec '[Pos]]
vertices = map (coord3d =:) $ L.V3 <$> [1,-1] <*> [1,-1] <*> [1,-1]

colors :: [V.PlainRec '[Color]]
colors = map (v_color =:) $ L.V3 <$> [1,-1] <*> [1,-1] <*> [1,-1]

elements :: [L.V3 GL.GLuint]
elements = [ L.V3 2 1 0
           , L.V3 1 2 3
           , L.V3 0 1 4
           , L.V3 4 1 5
           , L.V3 4 5 6
           , L.V3 7 6 5
           , L.V3 2 6 3
           , L.V3 6 3 7
           , L.V3 0 4 2
           , L.V3 2 4 6
           , L.V3 5 1 7
           , L.V3 7 1 3
           ]

animationWaitTime = 3

data Resources = Resources
                 { triProgram :: U.ShaderProgram
                 , buffer :: VG.BufferedVertices [Pos, Color]
                 , elementBuffer :: GL.BufferObject
                 }

type Pos = "coord3d" ::: L.V3 GL.GLfloat
type Color = "v_color" ::: L.V3 GL.GLfloat
type MVP = "mvp" ::: L.M44 GL.GLfloat

coord3d = V.Field :: Pos
v_color = V.Field :: Color
mvp = V.Field :: MVP
