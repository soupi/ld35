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
