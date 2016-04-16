module Main where

import Prelude
import Data.Traversable (traverse)
import Data.List (List(..), (!!), (:))
import Data.List ((!!)) as List
import Data.Array ((!!)) as Array
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Tuple (Tuple(..), fst)
import Graphics.Canvas as C
import Graphics.Drawing as Draw
import Signal (runSignal, foldp) as S
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Timer (TIMER)
import DOM (DOM)

import Input as Input
import Utils
import Zipper
import CanvasUtils
import Shape
import Model

import Debug.Trace

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

type State =
  { qas :: Zipper QA
  , score :: Number
  , answer :: Answer
  , wall :: Number
  }


initState :: State
initState =
  { qas: Zipper (qa "1+1=" (Tuple true "2" : Tuple false "1" : Tuple false "3" : Nil)) Nil Nil
  , score: 200.0
  , answer: 0
  , wall: -300.0
  }

tick :: Number
tick = speed / 60.0

speed :: Number
speed = height / 5.0


wallHeight :: Number
wallHeight = 200.0

------------
-- Update
------------


update :: Input.Input -> State -> State
update input state =
  let new = state.wall > height + 300.0
      wall = if new then -300.0 else state.wall + tick
      match = maybe false (not <<< fst) $ (List.!! state.answer) $ (current state.qas).answers
                 
      pos = height - state.score

      score =
        if    pos > wall
           && pos < wall + wallHeight
           && match
        then
          height - (wall + wallHeight)
        else
          state.score

      answer =
        if input.arrows.left == Input.Click then
          0
        else if input.arrows.up == Input.Click then
          1
        else if input.arrows.down == Input.Click then
          2
        else if input.arrows.right == Input.Click then
          3
        else
          state.answer
          
  in
      { qas: state.qas
      , wall: wall
      , score: score
      , answer: answer
      }

------------
-- Render
------------

render :: C.Context2D -> State -> Eff ( canvas :: C.Canvas | _) Unit
render context state = do
  clearCanvas context
  renderPlayer context state
  renderWall context state
  pure unit

renderPlayer :: C.Context2D -> State -> Eff ( canvas :: C.Canvas | _) Unit
renderPlayer context state =
  let pos = height - state.score
      shape =
        fromMaybe (circle {x: 500.0, y: pos} 30.0) $
          (Array.!! state.answer)
            [ circle {x: 500.0, y: pos} 30.0
            , triangle {x:500.0, y: pos} 60.0
            , square {x: 500.0, y: pos} 50.0
            ]
  in
      Draw.render context shape

renderWall :: C.Context2D -> State -> Eff ( canvas :: C.Canvas | _) Unit
renderWall context state =
  Draw.render context $
    daiKabe state.wall width wallHeight


clearCanvas ctx = do
  C.setFillStyle "#1B1C1B" ctx
  C.fillRect ctx { x: 0.0, y: 0.0, w: width + 100.0, h: height + 100.0 }


