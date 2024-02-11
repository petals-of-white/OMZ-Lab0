
module Main where

import           Control.Lens
import           Control.Monad               (unless)
import           Data.Binary as Binary (encode, decode)
import           Data.ByteString             as BS (length, reverse)
import           Data.ByteString.Char8       (unpack)
import           Data.ByteString.Lazy  as LBS       (fromStrict, append)
import           Data.DICOM                  as Dicom
import           Data.List                   (find)
import           Data.Maybe                  (fromJust)
import           Data.Word                   (Word16, Word32, Word8)
import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import           Prelude                     hiding (reverse)
import           System.Exit                 (exitSuccess)

findElement :: Tag -> Object -> Maybe Element
findElement t = find (\Element {elementTag = _t} -> _t == t) . runObject


-- findDataByTag :: Tag -> [Element] -> Maybe ByteString
-- findDataByTag t els = do
--   Element {elementContent = BytesContent content} <- findElement t els
--   return content

-- dicomObj :: forall a b. (Format a ~ b) => IO ((Word16, Word16), ByteString, b)
-- dicomObj =
--     (\els ->
--       let (row, _) = fromJust $ readWord16 =<< findDataByTag rowsT els
--           (col,_) = fromJust $ readWord16 =<< findDataByTag columnsT els
--           pixData = fromJust $ findDataByTag PixelData els in

--       ((row, col), pixData, (Format RFLoat))
--       -- ((readWord16 rowB, readWord16 colB), )
--     ) . runObject . either error id <$> readObjectFromFile "C:\\Users\\maxle\\Обробка_медичних_зображень\\Lab0\\DICOM_Image_8b.dcm"

--   where rowsT = tag (TagGroup 0x0028) (TagElement 0x0010)
--         columnsT = tag (TagGroup 0x0028) (TagElement 0x0011)

main :: IO ()
main =
  let findData t o = findElement t o >>=
        (\Element {elementContent = content} -> case content of
            BytesContent bytesContent -> Just bytesContent
            _                         -> Nothing)

  in do
  dicom <- either error id <$> readObjectFromFile "C:\\Users\\maxle\\Обробка_медичних_зображень\\Lab0\\DICOM_Image_8b.dcm"
  -- print $ runObject dicom
  (rows, columns, imgBytes) <- return $ fromJust $ do
      r :: Word16 <- decode . LBS.fromStrict . reverse <$> findData Rows dicom
      c :: Word16 <- decode . LBS.fromStrict . reverse <$> findData Columns dicom
      let size :: Int = fromIntegral r * fromIntegral c
      img :: [Word8] <- decode . LBS.append (encode size) . fromStrict <$> findData PixelData dicom
      -- rescaleInter <- unpack <$> findData RescaleIntercept dicom
      -- rescaleSlope <- unpack <$> findData RescaleSlope dicom
      -- case (rescaleInter, rescaleSlope) of
      --   ("0", "1") -> "GLFloat"
      return (r,c,img)
  putStrLn $
    "Rows: " ++ show rows ++ ". Columns: " ++ show columns ++ ". Image data: " ++ show (Prelude.length imgBytes)
  exitSuccess
  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColor RGB8) (GLFW.defaultWindowConfig "Checkers")
    vertexBuffer :: Buffer os (B2 Float) <- newBuffer 4
    writeBuffer vertexBuffer 0 [V2 0 0, V2 1 0, V2 0 1, V2 1 1]
    tex <- newTexture2D R8 (V2 8 8) 1
    let whiteBlack = cycle [minBound,maxBound] :: [Word32]
        blackWhite = tail whiteBlack
    writeTexture2D tex 0 0 (V2 8 8) (cycle (take 8 whiteBlack ++ take 8 blackWhite))

    colorTex <- newTexture2D RG8 (V2 256 256) 1
    depthTex <- newTexture2D Depth16 (V2 256 256) 1

    shader1 <- compileShader $ do
      texMappedFragmentStream <- getProjectedFragments 256 (V3 0.5 (-0.8) (-0.8)) (V3 0.5 0.5 0) (V3 0 1 0)  textureMappedPrimitives
      solidFragmentStream <- getProjectedFragments 256 (V3 (-0.6) (-0.6) 0.8) (V3 0.25 0.25 0) (V3 0 1 0) solidPrimitives
      let filter = SamplerFilter Nearest Nearest Nearest Nothing
          edge = (pure ClampToEdge, 0)
      samp <- newSampler2D (const (tex, filter, edge))
      let sampleTexture = sample2D samp SampleAuto Nothing Nothing
          texMappedFragmentStream2 = filterFragments ((>* 0.5) . sampleTexture) texMappedFragmentStream
          texMappedFragmentStream3 = fmap (const (V2 1 0)) texMappedFragmentStream2
          solidFragmentStream2 = fmap (const (V2 0 1)) solidFragmentStream
          fragmentStream = solidFragmentStream2 `mappend` texMappedFragmentStream3
          fragmentStream2 = withRasterizedInfo (\a r -> (a, rasterizedFragCoord r ^. _z)) fragmentStream
      drawDepth (\s -> (NoBlending, depthImage s, DepthOption Less True)) fragmentStream2 $ \ a -> do
        drawColor (\ s -> (colorImage s, pure True, False)) a

    shader2 <- compileShader $ do
      fragmentStream <- getProjectedFragments 800 (V3 1 2 2) (V3 0.5 0.5 0) (V3 0 1 0) id

      let filter = SamplerFilter Linear Linear Nearest Nothing
          edge = (pure ClampToEdge, 0)
      samp <- newSampler2D (const (colorTex, filter, edge))
      let sampleTexture = sample2D samp SampleAuto Nothing Nothing
          fragmentStream2 = fmap ((\(V2 r g) -> V3 r 0 g) . sampleTexture) fragmentStream
      drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) fragmentStream2

    renderLoop win [
      do
        vertexArray <- newVertexArray vertexBuffer
        let singleTriangle = takeVertices 3 vertexArray
        cImage <- getTexture2DImage colorTex 0
        dImage <- getTexture2DImage depthTex 0
        clearImageColor cImage 0
        clearImageDepth dImage 1
        shader1 $ ShaderEnvironment
            (toPrimitiveArray TriangleStrip vertexArray)
            (toPrimitiveArray TriangleList singleTriangle)
            cImage
            dImage
      ,
      do
        clearWindowColor win 0.5
        vertexArray <- newVertexArray vertexBuffer
        shader2 (toPrimitiveArray TriangleStrip vertexArray)
      ]

getProjectedFragments size eye center up sf = do
  primitiveStream <- toPrimitiveStream sf
  let primitiveStream2 = fmap (\pos2d -> (make3d eye center up pos2d, pos2d)) primitiveStream
  rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 size size), DepthRange 0 1)) primitiveStream2

make3d eye center up (V2 x y) = projMat !*! viewMat !* V4 x y 0 1
  where
    viewMat = lookAt' eye center up
    projMat = perspective (pi/3) 1 1 100

renderLoop win renderings = do
  mapM_ render renderings
  swapWindowBuffers win
  closeRequested <- GLFW.windowShouldClose win
  unless (closeRequested == Just True) $
    renderLoop win renderings

-- Copy of lookAt from linear with normalize replaced with signorm
lookAt' eye center up =
  V4 (V4 (xa^._x)  (xa^._y)  (xa^._z)  xd)
     (V4 (ya^._x)  (ya^._y)  (ya^._z)  yd)
     (V4 (-za^._x) (-za^._y) (-za^._z) zd)
     (V4 0         0         0          1)
  where za = signorm $ center - eye
        xa = signorm $ cross za up
        ya = cross xa za
        xd = -dot xa eye
        yd = -dot ya eye
        zd = dot za eye

data ShaderEnvironment = ShaderEnvironment
  {
    textureMappedPrimitives, solidPrimitives  :: PrimitiveArray Triangles (B2 Float),
    colorImage :: Image (Format RGFloat),
    depthImage :: Image (Format Depth)
  }
