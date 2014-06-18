module Points where

import Graphics.Rendering.OpenGL

points :: Int -> [(GLfloat, GLfloat, GLfloat)]
points n = [(sin (ang k), cos (ang k), 0) | k <- [1..n']]
    where n' = fromIntegral n
          ang k = 2 * pi * k / n'
