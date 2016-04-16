module Shape
  ( square
  , triangle
  , circle
  , daiKabe
  )
where

import Prelude (($), (+), (/))
import Math (pow, sqrt)
import Graphics.Drawing hiding (circle)
import Graphics.Drawing (circle) as Draw
import Color as Color


square :: Point -> Number -> Drawing
square loc size =
  filled (fillColor (Color.rgb 255 255 80)) $
    rectangle loc.x loc.y size size

triangle :: Point -> Number -> Drawing
triangle loc size =
  filled (fillColor (Color.rgb 80 255 80)) $
    closed
        [ { x: loc.x, y: loc.y + size }
        , { x: loc.x + base size, y: loc.y + size }
        , { x: loc.x + base size / 2.0, y: loc.y }
        ]

  where
    base x = sqrt ((x / 2.0) `pow` 2.0 + x `pow` 2.0)

circle :: Point -> Number -> Drawing
circle loc size =
  filled (fillColor (Color.rgb 0 130 255)) $
    Draw.circle loc.x loc.y size

daiKabe :: Number -> Number -> Number -> Drawing
daiKabe h ws hs =
  filled (fillColor (Color.rgb 80 40 130)) $
    rectangle 0.0 h ws hs
