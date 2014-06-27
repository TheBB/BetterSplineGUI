{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Rendering
       ( Canvas
       , ViewPort (Top, Side, Front, Perspective)
       , initGL
       , doRealize
       ) where

import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import Control.Applicative

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
import Linear ((!*!), (!!*))
import Graphics.Rendering.OpenGL (($=))

  
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
  GtkGL.glDrawingAreaNew glConfig


-- doRealize should be called when a canvas has been realized. It then allocates the necessary
-- OpenGL resources and sets up the proper redraw event callback.
doRealize :: GtkGL.GLDrawingArea -> IO ()
doRealize canvas = do
  GtkGL.withGLDrawingArea canvas $ \_ -> do
    program <- initResources
    Gtk.onExpose canvas $ \_ -> do
      GtkGL.withGLDrawingArea canvas $ \glwindow -> do
        draw program glwindow
        GtkGL.glDrawableSwapBuffers glwindow
      return True
  return ()


-- These types and singletons correspond to the attributes declared in the shaders.
-- This is Vinyl type masturbation, no need to worry much about it.
type Pos = "coord3d" ::: L.V3 GL.GLfloat
type Color = "v_color" ::: L.V3 GL.GLfloat
type MVP = "mvp" ::: L.M44 GL.GLfloat

coord3d = V.Field :: Pos
v_color = V.Field :: Color
mvp = V.Field :: MVP

 
-- The GLResources struct contains a shader program and various buffer objects.
data GLResources = GLResources
                   { glProgram :: U.ShaderProgram
                   , buffer :: VG.BufferedVertices [Pos, Color]
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
    <*> VG.bufferVertices (zipWith (<+>) vertices colors)
    <*> U.fromSource GL.ElementArrayBuffer elements

  where vSrc = $(embedFile "src/shader/vs.glsl") :: BS.ByteString
        fSrc = $(embedFile "src/shader/fs.glsl") :: BS.ByteString


-- The draw function refreshes an OpenGL window.
draw :: GLResources -> GtkGL.GLWindow -> IO ()
draw (GLResources prg buf eBuf) glwindow = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  
  -- Make sure resizing is properly dealt with.
  (width, height) <- GtkGL.glDrawableGetSize glwindow
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
  
  -- Elapsed time.
  t <- maybe 0 id <$> GLFW.getTime

  -- Draw.
  GL.currentProgram $= (Just . U.program $ prg)
  VG.enableVertices' prg buf
  VG.bindVertices buf
  VG.setAllUniforms prg $ transformM width height t
  GL.bindBuffer GL.ElementArrayBuffer $= Just eBuf
  U.drawIndexedTris (fromIntegral $ length elements)

  
-- transformM generates a transformation matrix to be passed to the vertex shader.
transformM :: Int -> Int -> Double -> V.PlainRec '[MVP]
transformM width height t = mvp =: (move !*! scale !*! proj !*! view !*! model !*! anim)
  where anim  = L.mkTransformation (L.axisAngle (L.V3 0 1 0) angle) L.zero
        model = L.mkTransformationMat L.eye3 $ L.V3 0 0 (-4)
        view  = U.camMatrix $ U.tilt (-30) . U.dolly (L.V3 0 2 0) $ U.fpsCamera
        proj  = U.projectionMatrix (pi/4) aspect 0.1 10
        scale = L.V4 (L.V4 0.5 0 0 0) (L.V4 0 0.5 0 0) (L.V4 0 0 1 0) (L.V4 0 0 0 1)
        move  = L.mkTransformation (L.axisAngle (L.V3 0 0 1) 0.0) (L.V3 0.5 0.5 0)

        angle = realToFrac t * pi / 4
        aspect = fromIntegral width / fromIntegral height

        
-- Vertex, color and element buffers to display a rainbow cube.
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
