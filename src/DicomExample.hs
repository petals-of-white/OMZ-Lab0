
module Main where
import           Control.Monad               (unless)
import           Data.Binary                 as Binary (decode, encode)
import qualified Data.ByteString             as BS
import           Data.ByteString.Lazy        as LBS (append, fromStrict)
import           Data.DICOM                  as Dicom
import           Data.List                   (find)
import           Data.Maybe                  (fromJust)
import           Data.Word                   (Word16, Word8)
import           Graphics.GPipe
import           Graphics.GPipe.Context.GLFW (WindowConfig (..))
import qualified Graphics.GPipe.Context.GLFW as GLFW
import           Prelude                     hiding (reverse)



main :: IO ()
main = do
  dicom <- either error id <$> readObjectFromFile "C:\\Users\\maxle\\Обробка_медичних_зображень\\Lab0\\DICOM_Image_8b.dcm"
  let ((rows, columns), imgBytes) = fromJust $ getDicomData dicom
  putStrLn $
    "Rows: " ++ show rows ++ ". Columns: " ++ show columns ++ ". Image data: " ++ show (Prelude.length imgBytes)

  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColor RGB8) ((GLFW.defaultWindowConfig "Dicom Test") {configWidth=rows, configHeight=columns})
    vertexBuffer :: Buffer os (B2 Float, B2 Float) <- newBuffer 4
    writeBuffer vertexBuffer 0 [(V2 (-1) (-1), V2 0 1), (V2 1 (-1), V2 1 1), (V2 (-1) 1, V2 0 0), (V2 1 1, V2 1 0)]
    let texSize = V2 rows columns
    tex <- newTexture2D R8 texSize 1
    writeTexture2D tex 0 0 texSize imgBytes

    shader <- compileShader $ do
      primitiveStream <- toPrimitiveStream id
      let primitiveStream2 = fmap (\(V2 x y, uv) -> (V4 x y 0 1, uv)) primitiveStream
      fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 rows columns), DepthRange 0 1)) primitiveStream2
      let --filter = SamplerFilter Nearest Nearest Nearest Nothing
          filter = SamplerNearest
          edge = (pure Mirror, undefined)
      samp <- newSampler2D (const (tex, filter, edge))
      let sampleTexture = pure . sample2D samp SampleAuto Nothing Nothing
          fragmentStream2 = fmap sampleTexture fragmentStream
      drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) fragmentStream2

    renderLoop win $ do
      clearWindowColor win 0
      vertexArray <- newVertexArray vertexBuffer
      shader (toPrimitiveArray TriangleStrip vertexArray)

renderLoop win rendering = do
  render rendering
  swapWindowBuffers win
  closeRequested <- GLFW.windowShouldClose win
  unless (closeRequested == Just True) $
    renderLoop win rendering

findElement :: Tag -> Object -> Maybe Element
findElement t = find (\Element {elementTag = _t} -> _t == t) . runObject

findData :: Tag -> Object -> Maybe BS.ByteString
findData t o = findElement t o >>=
        (\Element {elementContent = content} -> case content of
            BytesContent bytesContent -> Just bytesContent
            _                         -> Nothing)

getDicomData :: Object -> Maybe ((Int, Int), [Float])
getDicomData dicom = do
      r :: Word16 <- decode . LBS.fromStrict . BS.reverse <$> findData Rows dicom
      c :: Word16 <- decode . LBS.fromStrict . BS.reverse <$> findData Columns dicom
      let size :: Int = fromIntegral r * fromIntegral c
      img :: [Word8] <- decode . LBS.append (encode size) . fromStrict <$> findData PixelData dicom
      -- rescaleInter <- unpack <$> findData RescaleIntercept dicom
      -- rescaleSlope <- unpack <$> findData RescaleSlope dicom
      return ((fromIntegral r, fromIntegral c), map (\word -> realToFrac word / 255 :: Float) img)
