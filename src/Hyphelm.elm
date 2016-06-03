module Hyphelm
  exposing ( Language(..)
           , syllabize
           )

import String
import List

import Hyphelm.Types exposing (..)
import Hyphelm.Polish as PL

type Language
  = Polish

{-| Syllabizes given word according to rules in given language.

    syllabize Polish "absurd" == ["ab", "surd"]
-}
syllabize : Language -> String -> List String
syllabize lang word =
  case lang of
    Polish -> PL.syllabize word
