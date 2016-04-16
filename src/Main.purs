module Main where

import Prelude (pure, bind, ($), (<$>), Unit, unit)
import Data.Maybe (Maybe(..))
import Graphics.Canvas as C
import Signal (runSignal, foldp) as S
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Timer (TIMER)
import DOM (DOM)

import Input as Input
import Utils
import CanvasUtils


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
  pure unit

clearCanvas ctx = do
  C.setFillStyle "#1B1C1B" ctx
  C.fillRect ctx { x: 0.0, y: 0.0, w: width, h: height }


