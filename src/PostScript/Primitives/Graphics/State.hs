module PostScript.Primitives.Graphics.State where

import PostScript.Types (Interpreter)

-- Graphics state stack

-- 'gsave': push copy of current GraphicsState onto stack
psGsave :: Interpreter ()
psGsave = undefined

-- 'grestore': pop and restore previous GraphicsState
psGrestore :: Interpreter ()
psGrestore = undefined

-- 'grestoreall': restore bottom-most GraphicsState
psGrestoreall :: Interpreter ()
psGrestoreall = undefined

-- Color operators

-- 'setrgbcolor': r g b setrgbcolor -> set current stroke/fill color
psSetrgbcolor :: Interpreter ()
psSetrgbcolor = undefined

-- 'setgray': gray setgray -> set grayscale color (0=black, 1=white)
psSetgray :: Interpreter ()
psSetgray = undefined

-- 'setcmykcolor': c m y k setcmykcolor -> convert CMYK to RGB and set color
psSetcmykcolor :: Interpreter ()
psSetcmykcolor = undefined

-- 'sethsbcolor': h s b sethsbcolor -> convert HSB to RGB and set color
psSethsbcolor :: Interpreter ()
psSethsbcolor = undefined

-- State queries

-- 'currentrgbcolor': -> r g b (push current color components)
psCurrentrgbcolor :: Interpreter ()
psCurrentrgbcolor = undefined

-- 'currentgray': -> gray (push current gray value)
psCurrentgray :: Interpreter ()
psCurrentgray = undefined

-- 'currentlinewidth': -> width (push current line width)
psCurrentlinewidth :: Interpreter ()
psCurrentlinewidth = undefined

-- Stroke styling

-- 'setlinewidth': width setlinewidth -> set line width
psSetlinewidth :: Interpreter ()
psSetlinewidth = undefined

-- 'setlinecap': int setlinecap -> 0=butt, 1=round, 2=square
psSetlinecap :: Interpreter ()
psSetlinecap = undefined

-- 'setlinejoin': int setlinejoin -> 0=miter, 1=round, 2=bevel
psSetlinejoin :: Interpreter ()
psSetlinejoin = undefined

-- 'setdash': array offset setdash -> set dash pattern
psSetdash :: Interpreter ()
psSetdash = undefined

-- Painting & rendering actions

-- 'stroke': draw outlines of currentPath to canvas and clear path
psStroke :: Interpreter ()
psStroke = undefined

-- 'fill': fill interior of currentPath (nonzero rule) and clear path
psFill :: Interpreter ()
psFill = undefined

-- 'eofill': fill interior of currentPath (evenodd rule) and clear path
psEofill :: Interpreter ()
psEofill = undefined

-- 'clip': intersect clipping path with currentPath
psClip :: Interpreter ()
psClip = undefined

-- 'showpage': flush canvas to output and reset page
psShowpage :: Interpreter ()
psShowpage = undefined
