module Main where

import Prelude
import Control.Monad (when)
import Math (ceil, max)
import Data.Traversable (traverse)
import Data.List (List(..), (!!), (:))
import Data.List ((!!)) as List
import Data.Array ((!!)) as Array
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
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
  , done :: Boolean
  }


initState :: State
initState =
  { qas: Zipper
           (qa "1+1=" (Tuple true "2" : Tuple false "1" : Tuple false "3" : Nil))
           Nil
           (qa "1+1=" (Tuple false "1" : Tuple true "2" : Tuple false "3" : Nil) : Nil)
  , score: 300.0
  , answer: 0
  , wall: -300.0
  , done: false
  }

tick :: Number
tick = speed / 60.0

speed :: Number
speed = height / 5.0


wallHeight :: Number
wallHeight = 200.0

initPlayer :: Number
initPlayer = 300.0

initWall :: Number
initWall = -300.0

------------
-- Update
------------


update :: Input.Input -> State -> State
update input state =
  let new = state.wall > height + 300.0
      wall = if new then initWall else state.wall + tick
      match = maybe true (not <<< fst) $ (List.!! state.answer) $ (current state.qas).answers
                 
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

      doneAndQas = if new && score > 0.0 then next state.qas else Tuple (not state.done) state.qas
      done = not $ fst doneAndQas
      qas = snd doneAndQas
  in
      { qas: qas
      , wall: wall
      , score: if done then state.score else score
      , answer: answer
      , done: done
      }

------------
-- Render
------------

render :: C.Context2D -> State -> Eff ( canvas :: C.Canvas | _) Unit
render context state = do
  clearCanvas context
  renderPlayer context state
  renderWall context state
  renderText context state
  when (state.score < -50.0) (renderGameOver context state)
  when state.done (renderGameComplete context state)
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
            , ex {x: 500.0, y: pos} 50.0
            ]
  in
      Draw.render context shape

renderWall :: C.Context2D -> State -> Eff ( canvas :: C.Canvas | _) Unit
renderWall context state =
  Draw.render context $
    daiKabe state.wall width wallHeight

renderText :: C.Context2D -> State -> Eff ( canvas :: C.Canvas | _) Unit
renderText context state =
  Draw.render context $
    texts 26 pink { x: 20.0, y: 30.0 }
      (  show (position state.qas + if state.done then 1 else 0) <> " / " <> show (length state.qas)
      <> (" Score: " <> show (max 0.0 $ ceil state.score))
      )

renderGameOver :: C.Context2D -> State -> Eff ( canvas :: C.Canvas | _) Unit
renderGameOver context state =
  Draw.render context $
    texts 46 red { x: width / 2.0 - 100.0, y: height / 2.0 } "Game Over"

renderGameComplete :: C.Context2D -> State -> Eff ( canvas :: C.Canvas | _) Unit
renderGameComplete context state =
  Draw.render context $
    texts 46 green { x: width / 2.0 - 100.0, y: height / 2.0 - 100.0 } "Well Done!" <>
    texts 46 white { x: width / 2.0 - 180.0, y: height / 2.0 } ("Your Score is: " <> show (ceil state.score))

clearCanvas ctx = do
  C.setFillStyle "#1B1C1B" ctx
  C.fillRect ctx { x: 0.0, y: 0.0, w: width + 100.0, h: height + 100.0 }
