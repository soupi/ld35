module Sound
    ( Sound
    , SOUND
    , new
    , play
    ) where

import Prelude (Unit, (<<<))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

foreign import data SOUND :: !

foreign import data Sound :: *

foreign import getSound  :: forall e. String -> Eff (sound :: SOUND | e) Sound
foreign import playSound :: forall e. Sound  -> Eff (sound :: SOUND | e) Unit


new :: forall e. String -> Eff (sound :: SOUND | e) Sound
new = getSound

play :: forall e. Sound -> Aff (sound :: SOUND | e) Unit
play = liftEff <<< playSound
