{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Rendering
       ( Canvas
       , ViewPort (Top, Side, Front, Perspective)
       , initGL
       , doReady
       , doMaster
       ) where

import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import Control.Applicative
import Control.Monad.IO.Class (liftIO)

import qualified Graphics.GLUtil as U
import qualified Graphics.GLUtil.Camera3D as U
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.Gtk.OpenGL as GtkGL
import qualified Linear as L
import Linear ((!*!), (!!*))
import Graphics.Rendering.OpenGL (($=))
import Graphics.UI.Gtk as Gtk


data ViewPort = Top | Side | Front | Perspective
type Canvas = GtkGL.GLDrawingArea


-- initGL initializes GLFW (for timekeeping) and GtkGL. It does NOT initialize Gtk itself.
-- It then returns a canvas.
initGL :: IO Canvas
initGL = do
  GLFW.init
  GtkGL.initGL

  glConfig <- GtkGL.glConfigNew [ GtkGL.GLModeRGBA
                                , GtkGL.GLModeDepth
                                , GtkGL.GLModeDouble
                                ]
  GtkGL.glDrawingAreaNew glConfig Nothing


doReady :: [Canvas] -> IO ()
doReady [ca, cb] = do
  putStrLn "Both canvases are ready"
  -- GtkGL.withGLDrawingArea canvas $ \_ -> do
  --   program <- initResources
  --   Gtk.onExpose canvas $ \_ -> do
  --     GtkGL.withGLDrawingArea canvas $ \glwindow -> do
  --       draw program glwindow
  --       GtkGL.glDrawableSwapBuffers glwindow
  --     return True
  return ()


doMaster :: (Canvas -> IO ()) -> Table -> Canvas -> IO ()
doMaster post table cp = do
  putStrLn "Master canvas is ready"

  -- GtkGL.withGLDrawingArea cp $ \_ -> do
  --   program <- initResources
  --   cp `on` exposeEvent $ liftIO $ do
  --     GtkGL.withGLDrawingArea cp $ \glwindow -> do
  --       draw program glwindow
  --       GtkGL.glDrawableSwapBuffers glwindow
  --     return True
  --   return ()

  -- glConfig <- GtkGL.glConfigNew [ GtkGL.GLModeRGBA
  --                               , GtkGL.GLModeDepth
  --                               , GtkGL.GLModeDouble
  --                               ]
  -- ctx <- GtkGL.glDrawingAreaGetGLContext cp
  -- cs <- GtkGL.glDrawingAreaNew glConfig (Just ctx)

  postGUIAsync $ do
    cs <- buttonNewWithLabel "Hi there"
    cs `on` realize $ putStrLn "Slave realized"
    tableAttachDefaults table cs 1 2 0 1
    widgetShowAll table

  putStrLn "Made a new canvas shared"


-- The GLResources struct contains a shader program and various buffer objects.
data GLResources = GLResources
                   { glProgram :: U.ShaderProgram
                   , vertexBuffer :: GL.BufferObject
                   , colorBuffer :: GL.BufferObject
                   , elementBuffer :: GL.BufferObject
                   }


-- initResources sets some OpenGL state variables (the ones that won't change), and returns
-- a GLResources struct.
initResources :: IO GLResources
initResources = do
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.drawBuffer $= GL.BackBuffers
  GL.clearColor $= (GL.Color4 1 1 1 1)
  GL.depthFunc $= Just GL.Less

  GLResources
    <$> U.simpleShaderProgramBS vSrc fSrc
    <*> U.fromSource GL.ArrayBuffer vertices
    <*> U.fromSource GL.ArrayBuffer colors
    <*> U.fromSource GL.ElementArrayBuffer elements

  where vSrc = $(embedFile "src/shader/vs.glsl") :: BS.ByteString
        fSrc = $(embedFile "src/shader/fs.glsl") :: BS.ByteString


-- The draw function refreshes an OpenGL window.
draw :: GLResources -> GtkGL.GLWindow -> IO ()
draw (GLResources prg vBuf cBuf eBuf) glwindow = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  -- Make sure resizing is properly dealt with.
  (width, height) <- GtkGL.glDrawableGetSize glwindow
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

  -- Elapsed time.
  t <- maybe 0 id <$> GLFW.getTime

  -- Draw.
  GL.currentProgram $= (Just . U.program $ prg)

  U.enableAttrib prg "coord3d"
  U.enableAttrib prg "v_color"

  GL.bindBuffer GL.ArrayBuffer $= Just vBuf
  U.setAttrib prg "coord3d" GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0

  GL.bindBuffer GL.ArrayBuffer $= Just cBuf
  U.setAttrib prg "v_color" GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0

  U.asUniform (transformM width height t) $ U.getUniform prg "mvp"
  GL.bindBuffer GL.ElementArrayBuffer $= Just eBuf

  U.drawIndexedTris (fromIntegral $ length elements)

  GL.vertexAttribArray (U.getAttrib prg "coord3d") $= GL.Disabled
  GL.vertexAttribArray (U.getAttrib prg "v_color") $= GL.Disabled


-- transformM generates a transformation matrix to be passed to the vertex shader.
transformM :: Int -> Int -> Double -> L.M44 GL.GLfloat
transformM width height t = proj !*! view !*! model !*! anim
  where anim  = L.mkTransformation (L.axisAngle (L.V3 0 1 0) angle) L.zero
        model = L.mkTransformationMat L.eye3 $ L.V3 0 0 (-4)
        view  = U.camMatrix $ U.tilt (-30) . U.dolly (L.V3 0 2 0) $ U.fpsCamera
        proj  = U.projectionMatrix (pi/4) aspect 0.1 10

        angle = realToFrac t * pi / 4
        aspect = fromIntegral width / fromIntegral height


-- Vertex, color and element buffers to display a rainbow cube.
vertices :: [L.V3 Float]
vertices = L.V3 <$> [1,-1] <*> [1,-1] <*> [1,-1]

colors :: [L.V3 Float]
colors = L.V3 <$> [1,-1] <*> [1,-1] <*> [1,-1]

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
