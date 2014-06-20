{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Vector.Storable as V
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

  
initResources :: IO Program
initResources = do
  vs <- U.loadShader GL.VertexShader $ shaderPath </> "vs.glsl"
  fs <- U.loadShader GL.FragmentShader $ shaderPath </> "fs.glsl"
  p <- U.linkShaderProgram [vs, fs]
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  Program p <$> GL.get (GL.attribLocation p "coord2d")
  

draw :: Program -> GLFW.Window -> IO ()
draw (Program program attrib) win = do
  GL.clearColor $= GL.Color4 1 1 1 1
  GL.clear [GL.ColorBuffer]
  (width, height) <- GLFW.getFramebufferSize win
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

  GL.currentProgram $= Just program
  GL.vertexAttribArray attrib $= GL.Enabled
  V.unsafeWith vertices $ \ptr ->
    GL.vertexAttribPointer attrib $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 ptr)
  GL.drawArrays GL.Triangles 0 3
  GL.vertexAttribArray attrib $= GL.Disabled


data Program = Program GL.Program GL.AttribLocation

shaderPath :: FilePath
shaderPath = "src" </> "shader"

vertices :: V.Vector Float
vertices = V.fromList [  0.0,  0.8
                      , -0.8, -0.8
                      ,  0.8, -0.8
                      ]
