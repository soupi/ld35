module Model where

import Data.Tuple
import Data.List


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

{-
data Answer
  = One
  | Two
  | Three
  | Four
-}
