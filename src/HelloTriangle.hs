module Main where

import           Control.Monad               (unless)
import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Control.Arrow (Arrow(first))

main :: IO ()
main =
  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColor RGB8) (GLFW.defaultWindowConfig "Hello world!")

    vertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer 3
    writeBuffer vertexBuffer 0 [ (V4 (-1) 1 0 1, V3 1 0 0)
                               , (V4 0 (-1) 0 1, V3 0 1 0)
                               , (V4 1 1 0 1,  V3 0 0 1)
                               ]

    shader <- compileShader $ do
      primitiveStream <- toPrimitiveStream id
      let primitiveStream2 = fmap (\(pos,clr) -> (pos - V4 1 1 0 0, clr / 10)) primitiveStream  
      let primitiveStream3 = primitiveStream <> primitiveStream2  
      let rotationMatrix a = V4 (V4 (cos a) (-sin a) 0 0)   
                                (V4 (sin a) (cos a) 0 0)  
                                (V4 0    0    1 0)  
                                (V4 0    0    0 1)  
      let primitiveStream4 = fmap (first (rotationMatrix (-0.2) !*)) primitiveStream3   
                      
      fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 500 500), DepthRange 0 1)) primitiveStream4
      drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) fragmentStream

    loop vertexBuffer shader win

loop vertexBuffer shader win = do
  render $ do
    clearWindowColor win (V3 0 0 0)
    vertexArray <- newVertexArray vertexBuffer
    let primitiveArray = toPrimitiveArray TriangleList vertexArray
    shader primitiveArray
  swapWindowBuffers win

  closeRequested <- GLFW.windowShouldClose win
  unless (closeRequested == Just True) $
      loop vertexBuffer shader win
