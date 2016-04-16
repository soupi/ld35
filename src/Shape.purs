module Shape
  ( square
  , triangle
  , circle
  , ex
  , daiKabe
  )
where

import Prelude (($), (+), (-), (/), (<>))
import Math (pow, sqrt)
import Graphics.Drawing hiding (circle)
import Graphics.Drawing (circle) as Draw
import Color as Color


square :: Point -> Number -> Drawing
square loc size =
  filled (fillColor (Color.rgb 255 255 80)) $
    rectangle loc.x loc.y size size

triangle :: Point -> Number -> Drawing
triangle l size =
  filled (fillColor (Color.rgb 80 255 80)) $
    closed
        [ { x: loc.x, y: loc.y + size }
        , { x: loc.x + base, y: loc.y + size }
        , { x: loc.x + base / 2.0, y: loc.y }
        ]

  where
    base = sqrt ((size / 2.0) `pow` 2.0 + size `pow` 2.0)
    loc = { x: l.x + size - base, y: l.y }

circle :: Point -> Number -> Drawing
circle loc size =
  filled (fillColor (Color.rgb 0 130 255)) $
    Draw.circle (loc.x + size) (loc.y + size) size

ex :: Point -> Number -> Drawing
ex loc size =
  outlined (lineWidth 5.0 <> outlineColor (Color.rgb 255 50 100))
    (closed
        [ { x: loc.x, y: loc.y + size }
        , { x: loc.x + size, y: loc.y }
        ]) <>

  outlined (lineWidth 5.0 <> outlineColor (Color.rgb 255 50 100))
    (closed
        [ { x: loc.x, y: loc.y }
        , { x: loc.x + size, y: loc.y + size }
        ])




daiKabe :: Number -> Number -> Number -> Drawing
daiKabe h ws hs =
  filled (fillColor (Color.rgb 80 40 130)) $
    rectangle 0.0 h ws hs

