module Model where

import Data.Tuple
import Data.List
import Data.Foldable (foldr)


type QA =
  { question :: String
  , answers  :: List (Tuple Boolean String)
  }

qa :: String -> List (Tuple Boolean String) -> QA
qa q as = 
  { question: q
  , answers: as
  }

type Answer = Int


