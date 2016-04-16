module Utils where

import Data.Maybe
import Prelude (Show, show, (<>))
import Debug.Trace (trace)


spyShow :: forall a. Show a => String -> a -> a
spyShow desc x = trace (desc <> ": " <> show x) (\_ -> x)

maybeToArray :: forall a. Maybe a -> Array a
maybeToArray Nothing  = []
maybeToArray (Just x) = [x]

