module Shape
where

import Prelude (($), (+), (-), (/), (<>))
import Data.Monoid (mempty)
import Math (pow, sqrt)
import Graphics.Drawing hiding (circle)
import Graphics.Drawing (circle) as Draw
import Graphics.Drawing.Font
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
    Draw.circle (loc.x - 5.0 + size) (loc.y + size) size

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
  filled (fillColor (Color.rgb 80 40 130))
    (rectangle 0.0 h ws hs) <>
  filled (fillColor (Color.rgb 205 205 205))
    (rectangle (ws / 2.0 - 25.0) (h + hs - 80.0) 80.0 80.0)

texts :: Int -> Color.Color -> Point -> String -> Drawing
texts size clr loc =
  text (myfont size) loc.x loc.y (fillColor clr)


myfont :: Int -> Font
myfont sz = font serif sz mempty

pink :: Color.Color
pink = Color.rgb 255 50 90

red :: Color.Color
red = Color.rgb 255 0 0

green :: Color.Color
green = Color.rgb 80 255 80

white :: Color.Color
white = Color.rgb 255 255 255
