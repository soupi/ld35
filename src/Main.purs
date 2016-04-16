module Main where

import Prelude (pure, bind, ($), (<$>), Unit, unit)
import Data.Traversable (traverse)
import Data.Maybe (Maybe(..))
import Graphics.Canvas as C
import Graphics.Drawing as Draw
import Signal (runSignal, foldp) as S
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Timer (TIMER)
import DOM (DOM)

import Input as Input
import Utils
import CanvasUtils
import Shape


----------
-- Glue
----------

main :: forall e. Eff (console :: CONSOLE, dom :: DOM, timer :: TIMER, canvas :: C.Canvas | e) Unit
main = do
  Just canvas <- C.getCanvasElementById "canvas"
  context <- C.getContext2D canvas
  inn <- Input.input
  let game = S.foldp update initState inn
  S.runSignal (render context <$> game)

-----------
-- Model
-----------

type State = Input.Input

initState :: State
initState = Input.initInput

------------
-- Update
------------

update :: Input.Input -> State -> State
update x _ = x

------------
-- Render
------------

render :: C.Context2D -> State -> Eff ( canvas :: C.Canvas | _) Unit
render context state = do
  clearCanvas context
  traverse (Draw.render context)
    [ circle {x: 100.0, y: 100.0} 30.0
    , triangle {x:500.0, y: 100.0} 60.0
    , square {x: 250.0, y: 500.0} 50.0
    ]
  pure unit

clearCanvas ctx = do
  C.setFillStyle "#1B1C1B" ctx
  C.fillRect ctx { x: 0.0, y: 0.0, w: width, h: height }


