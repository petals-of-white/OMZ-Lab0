
module Main where
import           Control.Monad               (unless)
import           Data.Binary                 as Binary (decode, encode)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as BSChar
import           Data.ByteString.Lazy        as LBS (append, fromStrict)
import           Data.DICOM                  as Dicom
import           Data.List                   (delete, find)
import           Data.Maybe                  (fromJust)
import           Data.Word                   (Word16, Word8)
import           Graphics.GPipe
import           Graphics.GPipe.Context.GLFW (WindowConfig (..))
import qualified Graphics.GPipe.Context.GLFW as GLFW
import           Prelude                     hiding (reverse)
import Paths_Lab0 (getDataFileName)

main :: IO ()
main = do
  filename <- getDataFileName "DICOM_Image_8b.dcm"
  dicom <- either error id <$> readObjectFromFile filename
  let ((rows, columns), imgBytes, intercept, slope, bitsAlloc) = fromJust $ getDicomData dicom
      size = rows * columns
      imgWord8 :: [Word8] = (decode . LBS.append (encode size) . fromStrict) imgBytes
  putStrLn $
    "Rows: " ++ show rows ++ ". Columns: " ++ show columns ++ ". Intercept: "
    ++ show intercept ++ ". Slope: " ++ show slope ++ ". Bits allocated: " ++ show bitsAlloc

  case (intercept, slope, bitsAlloc) of
    (i, s, _) | i /= 0, s /= 0 -> putStrLn "gl float"
    (_,_, 8)                   -> putStrLn "gl byte"
    (_,_, 16)                  -> putStrLn "gl short"
    _                          -> error "Unrecognized type?"


  runContextT GLFW.defaultHandleConfig $ do
    -- buffer
    vertexBuffer :: Buffer os (B2 Float, B2 Float) <- newBuffer 4

    -- задамо у вершини у вигляді координата, uv-текстури
    writeBuffer vertexBuffer 0 [(V2 (-1) (-1), V2 0 1),  (V2 1 (-1), V2 1 1),
                               (V2 (-1) 1, V2 0 0),     (V2 1 1, V2 1 0)]

    -- Textures
    let texSize = V2 rows columns
    tex <- newTexture2D R8UI texSize 1
    writeTexture2D tex 0 0 texSize imgWord8


    win <- newWindow (WindowFormatColor RGB8) ((GLFW.defaultWindowConfig "Dicom Test") {configWidth=rows, configHeight=columns})

    -- Shaders
    shader <- compileShader $ do
      primitiveStream <- toPrimitiveStream id
      let primitiveStream2 = fmap (\(V2 x y, uv) -> (V4 x y 0 1, uv)) primitiveStream
      fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 rows columns), DepthRange 0 1)) primitiveStream2
      let --filter = SamplerFilter Nearest Nearest Nearest Nothing
          filter = SamplerNearest
          edge = (pure ClampToEdge, undefined)
      samp <- newSampler2D (const (tex, filter, edge))
      let sampleTexture = pure . sample2D samp SampleAuto Nothing Nothing
          fragmentStreamSampled = fmap sampleTexture fragmentStream
          -- перетворимо word8 на float
          fragmentStreamFloat :: FragmentStream (V3 (S F Float)) = fmap ((/ 255) . fmap toFloat) fragmentStreamSampled
      drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) fragmentStreamFloat

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



-- Data extraction
type Intercept = Float
type Slope = Float
type Size = (Int, Int)
type BitsAllocated = Word16

getDicomData :: Object -> Maybe (Size, BS.ByteString, Intercept, Slope, BitsAllocated)
getDicomData dicom = do
      r :: Word16 <- decode . LBS.fromStrict . BS.reverse <$> findData Rows dicom
      c :: Word16 <- decode . LBS.fromStrict . BS.reverse <$> findData Columns dicom
      bitsAllocated :: BitsAllocated <- decode . LBS.fromStrict . BS.reverse <$> findData BitsAllocated dicom
      img <- findData PixelData dicom
      rescaleInter :: Float <- read. delete '+' . BSChar.unpack <$> findData RescaleIntercept dicom
      rescaleSlope :: Float <- read . delete '+' . BSChar.unpack <$> findData RescaleSlope dicom
      return ((fromIntegral r, fromIntegral c), img, rescaleInter, rescaleSlope, bitsAllocated)

findElement :: Tag -> Object -> Maybe Element
findElement t = find (\Element {elementTag = _t} -> _t == t) . runObject

findData :: Tag -> Object -> Maybe BS.ByteString
findData t o = findElement t o >>=
        (\Element {elementContent = content} -> case content of
            BytesContent bytesContent -> Just bytesContent
            _                         -> Nothing)
