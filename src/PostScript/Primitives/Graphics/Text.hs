module PostScript.Primitives.Graphics.Text where

import PostScript.Types (Interpreter)

-- Font selection & sizing

-- 'findfont': name findfont -> font
psFindfont :: Interpreter ()
psFindfont = undefined

-- 'scalefont': font scale scalefont -> scaled font
psScalefont :: Interpreter ()
psScalefont = undefined

-- 'setfont': font setfont -> set active font in GraphicsState
psSetfont :: Interpreter ()
psSetfont = undefined

-- Text rendering & vector outlines

-- 'show': string show -> render string at currentPoint transformed by CTM
psShow :: Interpreter ()
psShow = undefined

-- 'charpath': string bool charpath -> append glyph outlines to currentPath
psCharpath :: Interpreter ()
psCharpath = undefined

-- Typography metrics & customization

-- 'stringwidth': string stringwidth -> wx wy (measure advance without drawing)
psStringwidth :: Interpreter ()
psStringwidth = undefined

-- 'makefont': font matrix makefont -> apply matrix to font
psMakefont :: Interpreter ()
psMakefont = undefined

-- 'definefont': name font definefont -> register custom font dictionary
psDefinefont :: Interpreter ()
psDefinefont = undefined
