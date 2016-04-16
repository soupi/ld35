module Input where

import Prelude
import Data.Tuple (Tuple(..))
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Control.Timer (TIMER)
import Data.Int (toNumber)

import Signal (Signal, foldp, sampleOn) as S
import Signal.DOM (keyPressed, mouseButton, mousePos, animationFrame) as S
import Graphics.Drawing (Point)

import CanvasUtils (makePoint)

type Input =
  { arrows :: Arrows BtnAction
  , mouse :: Mouse
  }

type Arrows a =
  { right :: a
  , left  :: a
  , down  :: a
  , up    :: a
  }

data ArrowKey
  = LeftArrow
  | RightArrow
  | UpArrow
  | DownArrow

data Mouse
  = Mouse Point BtnAction

data BtnAction
  = Click
  | Hold
  | Idle
  | Release

instance showBtnAction :: Show BtnAction where
  show Idle = "Idle"
  show Hold = "Hold"
  show Click = "Click"
  show Release = "Release"

instance eqBtnAction :: Eq BtnAction where
  eq Hold Hold = true
  eq Click Click = true
  eq Idle Idle = true
  eq Release Release = true
  eq _ _ = false

instance showMouse :: Show Mouse where
  show (Mouse p b) = "Mouse " ++ "(" ++ show p.x ++ ", " ++ show p.y ++ ") " ++ show b

instance showArrowKey :: Show ArrowKey where
  show UpArrow = "UpArrow"
  show LeftArrow = "LeftArrow"
  show DownArrow = "DownArrow"
  show RightArrow = "RightArrow"

showArrows :: Arrows BtnAction -> String
showArrows arrows =
  "Arrows "
  <> show arrows.left
  <> " "
  <> show arrows.down
  <> " "
  <> show arrows.up
  <> " "
  <> show arrows.right

showInput :: Input -> String
showInput i = "Input\n  " ++ showArrows i.arrows ++ "\n  " ++ show i.mouse

input :: forall e. Eff (dom :: DOM, timer :: TIMER | e) (S.Signal Input)
input = do
  frames <- S.animationFrame
  arrows <- arrowsSignal
  mouse <- mouseSignal
  let sig = S.sampleOn frames $
        Tuple
        <$> arrows
        <*> mouse
  pure $
    S.foldp updateInput initInput sig

initInput :: Input
initInput =
  { arrows:
      { right: Idle
      , left: Idle
      , down: Idle
      , up: Idle
      }
  , mouse: Mouse (makePoint 0.0 0.0) Idle
  }

updateInput :: Tuple (Arrows Boolean) (Tuple Point Boolean) -> Input -> Input
updateInput (Tuple arrI mouseI) state =
  { arrows: arrFold arrI state.arrows
  , mouse: mouseFold mouseI state.mouse
  }

mouseSignal :: forall e. Eff (dom :: DOM | e) (S.Signal (Tuple Point Boolean))
mouseSignal = do
  pos  <- map (\p -> { x: toNumber p.x, y: toNumber p.y }) <$> S.mousePos
  down <- S.mouseButton 0
  let sig = Tuple <$> pos <*> down
  pure sig

mouseFold :: Tuple Point Boolean -> Mouse -> Mouse
mouseFold (Tuple pos newB) (Mouse _ bState) = Mouse pos (btnStateUpdate newB bState)

arrFold :: Arrows Boolean -> Arrows BtnAction -> Arrows BtnAction
arrFold inp arrows =
  { right: btnStateUpdate inp.right arrows.right
  , left: btnStateUpdate inp.left arrows.left
  , down: btnStateUpdate inp.down arrows.down
  , up: btnStateUpdate inp.up arrows.up
  }

btnStateUpdate :: Boolean -> BtnAction -> BtnAction
btnStateUpdate false Hold = Release
btnStateUpdate false _    = Idle
btnStateUpdate true  Idle = Click
btnStateUpdate true  _    = Hold

arrowsSignal :: forall e. Eff (dom :: DOM | e) (S.Signal (Arrows Boolean))
arrowsSignal = do
  rightArrow <- S.keyPressed rightKeyCode
  leftArrow  <- S.keyPressed leftKeyCode
  downArrow  <- S.keyPressed downKeyCode
  upArrow    <- S.keyPressed upKeyCode
  pure $ { left: _, right: _, down: _, up: _ }
      <$>  leftArrow
      <*>  rightArrow
      <*>  downArrow
      <*>  upArrow

leftKeyCode :: Int
leftKeyCode = 49

rightKeyCode :: Int
rightKeyCode = 52

upKeyCode :: Int
upKeyCode = 50

downKeyCode :: Int
downKeyCode = 51

asNum :: Boolean -> Number
asNum b = if b then 1.0 else 0.0
