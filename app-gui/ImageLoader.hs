module ImageLoader
  ( loadReferencePicture,
    missingImagePlaceholder,
  )
where

import Codec.BMP (parseBMP)
import Codec.Picture
  ( Image (..),
    PixelRGBA8 (..),
    convertRGBA8,
    encodeBitmap,
    imageHeight,
    imageWidth,
    readImage,
  )
import Data.Vector.Storable qualified as V
import Data.Vector.Storable.Mutable qualified as VM
import Graphics.Gloss

-- | Loads an image file (.jpg, .png, .bmp) and converts it into a scaled Gloss Picture fitting target dimensions.
-- High-resolution scans (e.g. 300 DPI 2550x3300) are smoothly downsampled using area-averaging
-- before uploading to OpenGL to avoid GL_NEAREST point-sampling artifacts.
loadReferencePicture :: FilePath -> (Float, Float) -> IO (Maybe Picture)
loadReferencePicture path (targetW, targetH) = do
  res <- readImage path
  case res of
    Left _err -> pure Nothing
    Right dynImg -> do
      let rawImg = convertRGBA8 dynImg
          srcW = imageWidth rawImg
          srcH = imageHeight rawImg
          -- Calculate integer downsampling factor K so texture matches target canvas size
          kX = srcW `div` max 1 (round targetW)
          kY = srcH `div` max 1 (round targetH)
          k = max 1 (min kX kY)
          rgbaImg = if k >= 2 then downsampleKxRGBA8 k rawImg else rawImg
          w = fromIntegral (imageWidth rgbaImg) :: Float
          h = fromIntegral (imageHeight rgbaImg) :: Float
          bmpBytes = encodeBitmap rgbaImg
      case parseBMP bmpBytes of
        Left _err -> pure Nothing
        Right bmp -> do
          let scaleX = targetW / w
              scaleY = targetH / h
              -- Fit within bounds while preserving aspect ratio
              s = min scaleX scaleY
              pic = bitmapOfBMP bmp
          pure (Just (Scale s s pic))

-- | Fast integer-ratio box / area-averaging downsampler for RGBA8 Storable Vectors.
-- Averages every KxK block of pixels so fine pen strokes and line art are preserved as anti-aliased gray
-- rather than skipped/dropped by point sampling.
downsampleKxRGBA8 :: Int -> Image PixelRGBA8 -> Image PixelRGBA8
downsampleKxRGBA8 k (Image srcW srcH vec)
  | k <= 1 = Image srcW srcH vec
  | otherwise =
      let dstW = srcW `div` k
          dstH = srcH `div` k
          k2 = k * k
          dstLen = dstW * dstH * 4
          dstVec = V.create $ do
            mvec <- VM.new dstLen
            let loopY dy
                  | dy >= dstH = pure ()
                  | otherwise = do
                      let sy = dy * k
                      loopX dy sy 0
                      loopY (dy + 1)

                loopX dy sy dx
                  | dx >= dstW = pure ()
                  | otherwise = do
                      let sx = dx * k
                          accumBlock y x (r, g, b, a)
                            | y >= k = (r, g, b, a)
                            | x >= k = accumBlock (y + 1) 0 (r, g, b, a)
                            | otherwise =
                                let idx = ((sy + y) * srcW + (sx + x)) * 4
                                    pr = fromIntegral (vec `V.unsafeIndex` idx) :: Int
                                    pg = fromIntegral (vec `V.unsafeIndex` (idx + 1))
                                    pb = fromIntegral (vec `V.unsafeIndex` (idx + 2))
                                    pa = fromIntegral (vec `V.unsafeIndex` (idx + 3))
                                 in accumBlock y (x + 1) (r + pr, g + pg, b + pb, a + pa)

                          (sr, sg, sb, sa) = accumBlock 0 0 (0, 0, 0, 0)
                          dstIdx = (dy * dstW + dx) * 4

                      VM.unsafeWrite mvec dstIdx (fromIntegral (sr `div` k2))
                      VM.unsafeWrite mvec (dstIdx + 1) (fromIntegral (sg `div` k2))
                      VM.unsafeWrite mvec (dstIdx + 2) (fromIntegral (sb `div` k2))
                      VM.unsafeWrite mvec (dstIdx + 3) (fromIntegral (sa `div` k2))
                      loopX dy sy (dx + 1)

            loopY 0
            pure mvec
       in Image dstW dstH dstVec

-- | Placeholder picture displayed when no reference image exists for a test
missingImagePlaceholder :: String -> Picture
missingImagePlaceholder name =
  Pictures
    [ Translate (-140) 20 $ Color (greyN 0.3) $ Scale 0.14 0.14 $ Text "No Reference Image",
      Translate (-120) (-20) $ Color (greyN 0.5) $ Scale 0.10 0.10 $ Text ("(" ++ name ++ ")")
    ]
