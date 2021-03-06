module Main where

import Prelude
import Control.Monad (when, unless)
import Math (ceil, max)
import Data.Int (toNumber)
import Data.Traversable (traverse)
import Data.List (List(..), (!!), (:))
import Data.List (zipWith,(!!)) as List
import Data.Array (zipWith, (!!)) as Array
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.String (length) as Str
import Graphics.Canvas as C
import Graphics.Drawing as Draw
import Signal (runSignal, foldp) as S
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Aff (launchAff, runAff)
import Control.Timer (TIMER)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)

import Input as Input
import Utils
import Zipper
import CanvasUtils
import Shape
import Model
import Sound as Sound
import QAs as QAs

----------
-- Glue
----------

main :: forall e. Eff (ajax :: AJAX, err :: EXCEPTION, console :: CONSOLE, sound :: Sound.SOUND, dom :: DOM, timer :: TIMER, canvas :: C.Canvas | e) Unit
main = do
  Just canvas <- C.getCanvasElementById "canvas"
  context <- C.getContext2D canvas
  inn <- Input.input
  sounds <-
    { change: _, gameover: _, welldone: _ }
      <$> Sound.new "audio/gling.mp3"
      <*> Sound.new "audio/gameover.mp3"
      <*> Sound.new "audio/welldone.mp3"

  launchAff $ do
    qas <- QAs.getQAs
    let game = S.foldp update (initState qas) inn
    liftEff $ S.runSignal (render context sounds <$> game)

-----------
-- Model
-----------

data Sound
  = Change
  | GameOver
  | WellDone

type Sounds =
  { change   :: Sound.Sound
  , gameover :: Sound.Sound
  , welldone :: Sound.Sound
  }

type State =
  { qas :: Zipper QA
  , score :: Number
  , answer :: Answer
  , wall :: Number
  , done :: Boolean
  , starting :: Boolean
  , sound :: Maybe Sound
  }


initState :: Zipper QA -> State
initState qas =
  { qas: qas
  , score: 300.0
  , answer: 0
  , wall: -600.0
  , done: false
  , starting: true
  , sound: Nothing
  }

tick :: Number
tick = speed / 60.0

speed :: Number
speed = height / 6.0


wallHeight :: Number
wallHeight = 250.0

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
      starting = if new then false else state.starting
  in if state.starting then
       state { wall = wall, starting = starting }
     else
       updateGame input state

updateGame :: Input.Input -> State -> State
updateGame input state =
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
      sound =
        if state.done || state.score < -50.0 then
           Nothing
        else if not state.done && done then
           Just WellDone
        else if score < -50.0 then
           Just GameOver
        else if answer /= state.answer then
           Just Change
        else
           Nothing
  in
      { qas: qas
      , wall: wall
      , score: if done then state.score else score
      , answer: answer
      , done: done
      , starting: false
      , sound: sound
      }

------------
-- Render
------------

render :: C.Context2D -> Sounds -> State -> Eff ( sound :: Sound.SOUND, canvas :: C.Canvas | _) Unit
render context sounds state = do
  case state.sound of
    Nothing -> pure unit
    Just s  -> playSound sounds s
  clearCanvas context
  if state.starting
    then
      renderKeys context state
    else do
      renderPlayer context state
      renderWall context state
      unless state.done (renderQAs context state)
      renderText context state
      when (state.score < -50.0) (renderGameOver context state)
      when state.done (renderGameComplete context state)
  pure unit

playSound :: Sounds -> Sound -> Eff ( sound :: Sound.SOUND | _) Unit
playSound sounds s =
    runAff (const $ pure unit) (const $ pure unit) (Sound.play sound)
  where
    sound =
      case s of
        Change -> sounds.change
        WellDone -> sounds.welldone
        GameOver -> sounds.gameover

clearCanvas ctx = do
  C.setFillStyle "#1B1C1B" ctx
  C.fillRect ctx { x: 0.0, y: 0.0, w: width + 100.0, h: height + 100.0 }

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

renderQAs :: C.Context2D -> State -> Eff ( canvas :: C.Canvas | _) Unit
renderQAs context state = do
  void $ (traverse (Draw.render context) $
    List.zipWith (<>)
        ( circle   {x: 200.0, y: state.wall + 80.0} 30.0
        : triangle {x: 200.0, y: state.wall + 170.0} 60.0
        : square   {x: 650.0, y: state.wall + 80.0} 50.0
        : ex       {x: 650.0, y: state.wall + 170.0} 50.0
        : Nil
        ) $

        List.zipWith (\(Tuple x y) str -> texts 30 white { x: x, y: state.wall + y } str)
        ( Tuple 300.0 120.0
        : Tuple 300.0 210.0
        : Tuple 750.0 110.0
        : Tuple 750.0 210.0
        : Nil
        )
        (map snd (current state.qas).answers)
    )
  Draw.render context $
    texts 36 white { x: width / 2.0 - toNumber (Str.length (current state.qas).question * 9), y: state.wall + wallHeight + 40.0 } (current state.qas).question

renderKeys :: C.Context2D -> State -> Eff ( canvas :: C.Canvas | _) Unit
renderKeys context state = do
  Draw.render context $
    smallKabe state.wall width wallHeight <>
    texts 40 white { x: width / 2.0 - toNumber (Str.length "Dai Kabe by suppi" * 9), y: state.wall + wallHeight + 150.0 } "Dai Kabe by suppi " <>
    texts 36 white { x: width / 2.0 - toNumber (Str.length "The keys are: " * 10), y: state.wall + 40.0 } "The keys are: "
  void $ (traverse (Draw.render context) $
    Array.zipWith (<>)
        [ circle   {x: 200.0, y: state.wall + 80.0} 30.0
        , triangle {x: 200.0, y: state.wall + 170.0} 60.0
        , square   {x: 650.0, y: state.wall + 80.0} 50.0
        , ex       {x: 650.0, y: state.wall + 170.0} 50.0
        ] $

        Array.zipWith (\(Tuple x y) str -> texts 30 white { x: x, y: state.wall + y } str)
        [ Tuple 300.0 120.0
        , Tuple 300.0 210.0
        , Tuple 750.0 110.0
        , Tuple 750.0 210.0
        ]
        [ " = 1"
        , " = 2"
        , " = 3"
        , " = 4"
        ]
    )
