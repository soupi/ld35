module QAs where

import Zipper
import Data.List
import Data.Tuple

import Model

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
    : Nil
    )
