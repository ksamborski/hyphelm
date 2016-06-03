module Hyphelm.Types
  exposing ( Phone(..)
           , phoneIsC
           , mergePhones
           , equalPhones
           , phonesToString
           )

import String
import List

type Phone
  = Vowel String
  | Consonant String

phoneIsC : Phone -> Bool
phoneIsC ph =
  case ph of
    Vowel _ -> False
    _ -> True

mergePhones : List Phone -> Phone
mergePhones phs =
  if List.isEmpty (List.filter
                        (\p -> case p of
                                Vowel _ -> True
                                _ -> False)
                        phs)
     then Consonant (phonesToString phs)
     else Vowel (phonesToString phs)

equalPhones : Phone -> Phone -> Bool
equalPhones p1 p2 =
  case (p1, p2) of
    (Vowel v1, Vowel v2) -> (String.toLower v1) == (String.toLower v2)
    (Consonant c1, Consonant c2) -> (String.toLower c1) == (String.toLower c2)
    _ -> False

phonesToString : List Phone -> String
phonesToString phs =
  String.concat
  <| List.map
      (\ph -> case ph of
        Vowel v -> v
        Consonant c -> c)
      phs

