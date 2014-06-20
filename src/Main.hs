{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.ByteString as BS
import Control.Applicative
import Data.FileEmbed (embedFile)
import System.FilePath ((</>))
  
import qualified Data.Vinyl as V
import qualified Graphics.GLUtil as U
import qualified Graphics.GLUtil.Camera3D as U
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.VinylGL as VG
import qualified Linear as L
import Data.Vinyl ((:::), (=:), (<+>))
import Graphics.Rendering.OpenGL (($=))

import qualified Util.GLFW as W

 
main :: IO ()
main = do
  win <- W.initialize "My First Triangle"
  program <- initResources
  W.mainLoop (draw program win) win
  W.cleanup win

vSrc = $(embedFile "src/shader/vs.glsl") :: BS.ByteString
fSrc = $(embedFile "src/shader/fs.glsl") :: BS.ByteString
 
initResources :: IO Resources
initResources = do
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  Resources
    <$> U.simpleShaderProgramBS vSrc fSrc
    <*> VG.bufferVertices (zipWith (<+>) vertices colors)
    <*> U.fromSource GL.ElementArrayBuffer elements

 
draw :: Resources -> GLFW.Window -> IO ()
draw (Resources s vb e) win = do
  GL.clearColor $= GL.Color4 1 1 1 1
  GL.depthFunc $= Just GL.Less
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
 
  (width, height) <- GLFW.getFramebufferSize win
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

  t <- maybe 0 id <$> GLFW.getTime

  GL.currentProgram $= (Just . U.program $ s)
  VG.enableVertices' s vb
  VG.bindVertices vb
  VG.setAllUniforms s $ transformM width height t
  GL.bindBuffer GL.ElementArrayBuffer $= Just e
  U.drawIndexedTris (fromIntegral $ length elements)


data Resources = Resources
                 { shaderProgram :: U.ShaderProgram
                 , buffer :: VG.BufferedVertices [Pos, Color]
                 , elementBuffer :: GL.BufferObject
                 }

transformM :: Int -> Int -> Double -> V.PlainRec '[MVP]
transformM width height t = mvp =: (proj L.!*! view L.!*! model L.!*! anim)
  where anim  = L.mkTransformation (L.axisAngle (L.V3 0 1 0) angle) L.zero
        model = L.mkTransformationMat L.eye3 $ L.V3 0 0 (-4)
        view  = U.camMatrix $ U.tilt (-30) . U.dolly (L.V3 0 2 0) $ U.fpsCamera
        proj  = U.projectionMatrix (pi/4) aspect 0.1 10

        angle = realToFrac t * pi / 4
        aspect = fromIntegral width / fromIntegral height
 

shaderPath :: FilePath
shaderPath = "src" </> "shader"

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

type Pos = "coord3d" ::: L.V3 GL.GLfloat
type Color = "v_color" ::: L.V3 GL.GLfloat
type MVP = "mvp" ::: L.M44 GL.GLfloat

coord3d = V.Field :: Pos
v_color = V.Field :: Color
mvp = V.Field :: MVP
