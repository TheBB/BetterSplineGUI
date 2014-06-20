{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import System.FilePath ((</>))
  
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GLUtil as U
import Graphics.Rendering.OpenGL (($=))

import qualified Util.GLFW as W

  
main :: IO ()
main = do
  win <- W.initialize "My First Triangle"
  program <- initResources
  W.mainLoop (draw program win) win
  W.cleanup win

  
initResources :: IO Resources
initResources = do
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  let v = shaderPath </> "vs.glsl"
      f = shaderPath </> "fs.glsl"
  Resources
    <$> U.simpleShaderProgram v f
    <*> U.makeBuffer GL.ArrayBuffer vertices
    <*> U.makeBuffer GL.ArrayBuffer colors
    

draw :: Resources -> GLFW.Window -> IO ()
draw r win = do
  GL.clearColor $= GL.Color4 1 1 1 1
  GL.clear [GL.ColorBuffer]
  (width, height) <- GLFW.getFramebufferSize win
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

  t <- maybe 0 id <$> GLFW.getTime
  let fade :: GL.Index1 GL.GLfloat
      fade = GL.Index1 . realToFrac $ sin (t*2*pi/5)/2 + 0.5

  GL.currentProgram $= (Just . U.program . triProgram $ r)
  U.enableAttrib (triProgram r) "coord2d"
  U.enableAttrib (triProgram r) "v_color"
  
  GL.bindBuffer GL.ArrayBuffer $= Just (vertBuffer r)
  U.setAttrib (triProgram r) "coord2d" GL.ToFloat $
    GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0
  
  GL.bindBuffer GL.ArrayBuffer $= Just (colorBuffer r)
  U.setAttrib (triProgram r) "v_color" GL.ToFloat $
    GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0

  U.setUniform (triProgram r) "fade" fade
  
  GL.drawArrays GL.Triangles 0 3
  GL.vertexAttribArray (U.getAttrib (triProgram r) "coord2d") $= GL.Disabled
  GL.vertexAttribArray (U.getAttrib (triProgram r) "v_color") $= GL.Disabled


data Resources = Resources
                 { triProgram :: U.ShaderProgram
                 , vertBuffer :: GL.BufferObject
                 , colorBuffer :: GL.BufferObject
                 }
                 

shaderPath :: FilePath
shaderPath = "src" </> "shader"
             
vertices :: [Float]
vertices = [  0.0,  0.8
           , -0.8, -0.8
           ,  0.8, -0.8
           ]
           
colors :: [Float]
colors = [ 1, 1, 0
         , 0, 0, 1
         , 1, 0, 0
         ]
