import Graphics.UI.GLUT
import Data.IORef

import Bindings

main :: IO ()
main = do
  (progName, args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  window <- createWindow "Hello World"

  angle <- newIORef 0
  delta <- newIORef 0.1
  pos <- newIORef (0, 0)

  depthFunc $= Just Less
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouse delta pos)
  displayCallback $= display angle pos
  idleCallback $= Just (idle angle delta)

  mainLoop
