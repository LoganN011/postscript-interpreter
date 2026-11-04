module PostScript.Primitives.Graphics.Image where

import PostScript.Types (Interpreter, Matrix, Object)

-- Bitmap image rasterization primitives

-- 'image': width height bits/comp matrix proc|string image -> render grayscale bitmap
psImage :: ([Object] -> Interpreter ()) -> Interpreter ()
psImage _evalProg = undefined

-- 'imagemask': width height polarity matrix proc|string imagemask -> render 1-bit mask
psImagemask :: ([Object] -> Interpreter ()) -> Interpreter ()
psImagemask _evalProg = undefined

-- 'colorimage': width height bits/comp matrix proc(s) multiproc ncolors colorimage -> render RGB/CMYK bitmap
psColorimage :: ([Object] -> Interpreter ()) -> Interpreter ()
psColorimage _evalProg = undefined

-- Helpers to decode hex streams and construct SVG <image> tags
decodeHexStream :: String -> [Int]
decodeHexStream = undefined

buildSVGImageElement :: Int -> Int -> Matrix -> [Int] -> String
buildSVGImageElement = undefined
