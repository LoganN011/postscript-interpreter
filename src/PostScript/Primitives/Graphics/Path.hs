module PostScript.Primitives.Graphics.Path where

import PostScript.Types (Interpreter)

-- Path construction primitives

-- 'newpath': clears currentPath and resets currentPoint
psNewpath :: Interpreter ()
psNewpath = undefined

-- 'moveto': x y moveto -> set current position
psMoveto :: Interpreter ()
psMoveto = undefined

-- 'rmoveto': dx dy rmoveto -> relative move from currentPoint
psRmoveto :: Interpreter ()
psRmoveto = undefined

-- 'lineto': x y lineto -> add line segment
psLineto :: Interpreter ()
psLineto = undefined

-- 'rlineto': dx dy rlineto -> relative line from currentPoint
psRlineto :: Interpreter ()
psRlineto = undefined

-- 'curveto': x1 y1 x2 y2 x3 y3 curveto -> add cubic bezier curve
psCurveto :: Interpreter ()
psCurveto = undefined

-- 'rcurveto': dx1 dy1 dx2 dy2 dx3 dy3 rcurveto -> relative bezier curve
psRcurveto :: Interpreter ()
psRcurveto = undefined

-- 'arc': x y r a1 a2 arc -> add counter-clockwise arc
psArc :: Interpreter ()
psArc = undefined

-- 'arcn': x y r a1 a2 arcn -> add clockwise arc
psArcn :: Interpreter ()
psArcn = undefined

-- 'closepath': close current subpath
psClosepath :: Interpreter ()
psClosepath = undefined

-- Path query & inspection primitives

-- 'currentpoint': -> x y (push current position)
psCurrentpoint :: Interpreter ()
psCurrentpoint = undefined

-- 'pathbbox': -> llx lly urx ury (push path bounding box)
psPathbbox :: Interpreter ()
psPathbbox = undefined

-- 'arcto': x1 y1 x2 y2 r arcto -> add tangent arc and push tangent points
psArcto :: Interpreter ()
psArcto = undefined

-- 'flattenpath': converts curves in path to line segments
psFlattenpath :: Interpreter ()
psFlattenpath = undefined

-- 'pathforall': move line curve close pathforall -> iterate path segments
psPathforall :: Interpreter ()
psPathforall = undefined
