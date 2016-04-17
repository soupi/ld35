module QAs where

import Prelude

import Zipper
import Data.List hiding (uncons)
import Data.Array (uncons)
import Data.Maybe
import Data.Either (Either(..), either)
import Data.Tuple
import Data.Foldable (foldr)
import Data.URI.Query (parseQuery, Query(..))
import Network.HTTP.Affjax
import Network.HTTP.StatusCode
import Data.Foreign
import Data.Foreign.Class
import Control.Monad.Aff
import Control.Monad.Eff.Class (liftEff, MonadEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad
import Control.Apply ((*>))
import Control.Bind ((=<<), join)
import Text.Parsing.StringParser (runParser)
import DOM.HTML.Location (search)
import DOM.HTML.Window (location)
import DOM.HTML (window)
import DOM (DOM)
import Data.StrMap as M

import Model
import Utils


qas :: Zipper QA
qas =
  Zipper
    ( qa "1 + 1 ="
         ( Tuple true  "2"
         : Tuple false "3"
         : Nil
         )
    )
    Nil
    ( qa "2 * 2 ="
         ( Tuple false "8"
         : Tuple true  "4"
         : Tuple false "16"
         : Nil
         )
    : qa "3 * 3 ="
         ( Tuple false "6"
         : Tuple false "15"
         : Tuple false "1"
         : Tuple true  "9"
         : Nil
         )
    : qa "5 * 8 ="
         ( Tuple false "45"
         : Tuple false "35"
         : Tuple true  "40"
         : Tuple false "38"
         : Nil
         )
    : qa "2 * 4 + 3 * 2 ="
         ( Tuple true  "14"
         : Tuple false "22"
         : Tuple false "12"
         : Tuple false "7"
         : Nil
         )
    : qa "35 - 16 ="
         ( Tuple false "21"
         : Tuple true  "19"
         : Tuple false "18"
         : Tuple false "29"
         : Nil
         )
    : qa "-4 * 3 + 15 ="
         ( Tuple false "1"
         : Tuple false "5"
         : Tuple true  "3"
         : Tuple false "-3"
         : Nil
         )
    : Nil
    )

data Obj = Obj
  { q  :: String
  , as :: Array Ans
  }

toQA :: Obj -> QA
toQA (Obj {q: q, as: as}) =
  { question: q
  , answers: foldr Cons Nil (map (\(Ans x) -> x) as)
  }

toQAs :: Array QA -> Maybe (Zipper QA)
toQAs as = case uncons as of
  Just {head: x, tail: xs} -> Just $ fromArray x xs
  Nothing -> Nothing

newtype Ans = Ans (Tuple Boolean String)

instance ansIsForeign :: IsForeign Ans where
  read value = do
    q <- readProp "correct" value
    v <- readProp "value" value
    pure $ Ans (Tuple q v)

instance objIsForeign :: IsForeign Obj where
  read value = do
    q <- readProp "question" value
    as <- readProp "answers" value
    pure $ Obj { q: q, as: as }


instance ansShow :: Show Ans where
  show (Ans value) =
    show value

instance objShow :: Show Obj where
  show (Obj {q:q,as:as}) =
    "{ question: " ++ q ++ ", answers: " ++ show as ++ " }"



getQAs
  :: forall e. Aff ( ajax :: AJAX, dom :: DOM, console :: CONSOLE | e)
          (Zipper
             { question :: String
             , answers :: List (Tuple Boolean String)
             })
getQAs = do
  rf <- liftEff (search =<< location =<< window)
  case (\(Query m) -> join $ M.lookup "?url" m) =<< either (const Nothing) Just (runParser parseQuery rf) of
    Nothing -> pure qas
    Just li -> do
      res <- get li
      case readJSON res.response of
        Left err -> liftEff (log (show err) *> pure qas)
        Right rs -> liftEff ((log $ "Loaded.") *> pure (fromMaybe qas (toQAs $ map toQA rs)))

