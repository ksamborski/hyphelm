module Hyphelm exposing (syllabize)

import String
import List
import Set

type Phone
  = Vowel String
  | Consonant String

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

unpronounceableMass : Set.Set String
unpronounceableMass = 
  Set.fromList
  [ "bc", "bch", "pc", "bd", "pd", "bs"
  , "ps", "cs", "rs", "nd", "nk", "lb"
  , "rd", "js", "tk", "kt", "nc", "jn"
  , "nt", "tn", "ls", "bg", "czn", "wn"
  , "ms", "ln", "jcz", "ck", "kc", "bn"
  , "nb", "lw", "rb", "jk", "rc", "zk"
  , "ks", "ld", "rh", "żb", "zt", "szm"
  , "mcz", "lf", "cht", "chm", "zd", "sf"
  , "ct", "dw", "cp", "rk", "dk", "nz"
  , "pt", "dm", "czk", "jsz", "wk", "nw"
  , "gc", "rn", "wd", "rf", "rp", "km"
  , "mp", "rch", "gf", "lk", "lh", "lg"
  , "lm", "żk", "mk", "lc", "dg", "rm"
  , "lch", "mb", "ndz", "dcz", "rcz"
  , "jz", "lt", "lz", "dzk", "ns", "szcz"
  , "szk", "wc", "rt", "rw", "lp", "ts"
  , "łcz", "mr", "ng", "rg", "nh", "mf"
  , "mh", "łk", "sb", "jd", "nż", "lcz"
  , "szt", "rł", "rsz", "tb", "tm", "td"
  , "gm", "rdz", "np", "lr", "fk", "jg"
  , "szw", "mn", "bk", "gs", "tcz", "nsz"
  , "nch", "źk", "fsz", "jc", "jr", "łż"
  , "źn", "rż", "zcz", "zj", "db", "zp"
  , "zsz", "dn", "zż", "rzm", "źdz", "gb"
  , "mz", "nf", "chn", "gh", "łt", "jt"
  , "kd", "gd", "źl", "df", "jl"
  , "dzl", "nm", "hl", "tf", "kcz", "rś"
  , "łl", "pn", "śk", "wl", "rl", "ncz"
  , "szn", "łdz", "pk", "ćm", "ść", "ćc"
  , "jż", "żn", "zw", "wski", "wska"
  , "ński", "jski", "lski", "łm", "kb"
  , "jm", "rski", "ńska", "jska", "lska", "rska"
  , "mski", "mska"
  ]

unpronounceablePhones : Phone -> Phone -> Bool
unpronounceablePhones p1 p2 =
  case (p1, p2) of
    (Consonant c1, Consonant c2) ->
      let (c1', c2') = (String.toLower c1, String.toLower c2)
      in List.member c1' ["ń"]
         || List.member c2' ["ń"]
         || Set.member (c1' ++ c2') unpronounceableMass
    (Consonant c1, Vowel c2) ->
      let (c1', c2') = (String.toLower c1, String.toLower c2)
      in List.member c1' ["ń"]
         || List.member c2' ["ń"]
         || Set.member (c1' ++ c2') unpronounceableMass
    (Vowel c1, Consonant c2) ->
      let (c1', c2') = (String.toLower c1, String.toLower c2)
      in List.member c1' ["ń"]
         || List.member c2' ["ń"]
         || Set.member (c1' ++ c2') unpronounceableMass
    _ -> False

isVowel : Char -> Bool
isVowel s =
  List.member (String.toLower <| String.fromChar s) ["a", "ą", "e", "ę", "i", "o", "u", "ó", "y"]

phonesToString : List Phone -> String
phonesToString phs =
  String.concat
  <| List.map
      (\ph -> case ph of
        Vowel v -> v
        Consonant c -> c)
      phs

phones : String -> List Phone
phones word =
  List.reverse
    <| String.foldl
        (\l acc ->
          if isVowel l
             then Vowel (String.fromChar l) :: acc
             else Consonant (String.fromChar l) :: acc)
        []
        word

indivisibleVowelsMass : Set.Set String
indivisibleVowelsMass =
  Set.fromList ["au", "eu", "ia", "ią", "ie", "ię", "io", "iu", "ió", "ue", "ay", "ski", "ska", "ipół", "dzia", "dzie", "dzią", "dzię", "dzi"]

indivisibleVowels : List Phone -> Bool
indivisibleVowels phs =
  Set.member
    (String.toLower <| phonesToString phs)
    indivisibleVowelsMass

indivisibleConsMass : Set.Set String
indivisibleConsMass = 
  Set.fromList ["cz", "rz", "sz", "dz", "ch"]

indivisibleConsonants : List Phone -> Bool
indivisibleConsonants phs =
  Set.member
    (String.toLower <| phonesToString phs)
    indivisibleConsMass

mapIndivisibles : List Phone -> List Phone
mapIndivisibles phs =
  List.reverse
  <| (\(prev, rest) ->
        case List.length prev of
          0 -> rest
          1 -> prev ++ rest
          2 -> if indivisibleVowels prev || indivisibleConsonants prev
                  then mergePhones prev :: rest
                  else (List.reverse prev) ++ rest
          3 -> let two1 = List.take 2 prev
                   two2 = List.drop 1 prev
               in if indivisibleVowels prev || indivisibleConsonants prev
                  then mergePhones prev :: rest
                  else if indivisibleVowels two1 || indivisibleConsonants two1
                          then (List.drop 2 prev) ++ (mergePhones two1 :: rest)
                          else if indivisibleVowels two2 || indivisibleConsonants two2
                                  then (mergePhones two2) :: (List.take 1 prev ++ rest)
                                  else (List.reverse prev) ++ rest
          4 -> let three1 = List.take 3 prev
                   three2 = List.drop 1 prev
                   two1   = List.take 2 prev
                   two2   = List.take 2 <| List.drop 1 prev
                   two3   = List.drop 2 prev
               in if indivisibleVowels prev || indivisibleConsonants prev
                  then mergePhones prev :: rest
                  else if indivisibleVowels three1 || indivisibleConsonants three1
                          then (List.drop 2 prev) ++ (mergePhones three1 :: rest)
                          else if indivisibleVowels three2 || indivisibleConsonants three2
                                  then (mergePhones three2) :: (List.take 1 prev ++ rest)
                                  else if (indivisibleVowels two1 || indivisibleConsonants two1) && (indivisibleVowels two3 || indivisibleConsonants two3)
                                          then mergePhones two3 :: mergePhones two1 :: rest
                                          else if indivisibleVowels two1 || indivisibleConsonants two1
                                                  then (List.reverse (List.drop 2 prev)) ++ (mergePhones two1 :: rest)
                                                  else if indivisibleVowels two2 || indivisibleConsonants two2
                                                          then (List.drop 3 prev) ++ (mergePhones two2 :: (List.take 1 prev ++ rest))
                                                          else if indivisibleVowels two3 || indivisibleConsonants two3
                                                                  then mergePhones two3 :: (List.reverse (List.take 2 prev) ++ rest)
                                                                  else (List.reverse prev) ++ rest
          _ -> rest)
  <| List.foldl
    (\ph (prev, acc) ->
      if List.length prev > 2
        then
          let two = List.take 2 prev
              three = List.take 3 prev
              four = prev ++ [ph]
          in if indivisibleVowels two || indivisibleConsonants two
            then (List.drop 2 prev ++ [ph], mergePhones two :: acc)
            else if indivisibleVowels three || indivisibleConsonants three
                    then (List.drop 3 prev ++ [ph], mergePhones three :: acc)
                    else if indivisibleVowels four || indivisibleConsonants four
                            then (List.drop 4 prev ++ [ph], mergePhones four :: acc)
                            else (List.drop 1 prev ++ [ph], List.take 1 prev ++ acc)

        else (prev ++ [ph], acc)) 
    ([], [])
    phs

filterHyphens : List Phone -> List Phone
filterHyphens phs =
  List.filter
    (\ph -> case ph of
      Consonant "-" -> False
      _ -> True)
    phs

divideByVowel : List Phone -> List (List Phone)
divideByVowel phs =
  List.reverse 
  <| (\(crest, rest) ->
      if List.isEmpty crest
         then rest
         else case (List.head rest, List.tail rest) of
                (Just hrest, Just trest) -> (hrest ++ (List.reverse crest)) :: trest
                (Just hrest, Nothing) -> [hrest ++ (List.reverse crest)]
                _ -> [List.reverse crest])
  <| List.foldl
      (\ph (current, acc) ->
        case ph of
          Vowel _ -> ([], (List.reverse <| ph :: current) :: acc)
          Consonant _ -> (ph :: current, acc))
      ([], [])
      phs

separate : (Phone -> Phone -> Bool) -> List Phone -> (List Phone, List Phone)
separate f phs =
  (\(prevph, toprev, tonew, stop) ->
    case prevph of
      Just p -> (List.reverse toprev, List.reverse <| p :: tonew)
      Nothing -> (List.reverse toprev, List.reverse tonew))
  <| List.foldl
      (\p (prevph, toprev, tonew, stop) ->
        case prevph of
          Just prev ->
            if stop
               then (Nothing, toprev, p :: prev :: tonew, True)
               else case p of
                 Vowel _ -> if f prev p
                               then (Nothing, prev :: (tonew ++ toprev), [p], True)
                               else (Nothing, toprev, p :: prev :: tonew, True)
                 _ -> if f prev p
                         then (Just p, prev :: (tonew ++ toprev), [], False)
                         else (Just p, toprev, prev :: tonew, not (List.isEmpty toprev))
          Nothing ->
            if stop
               then (Nothing, toprev, p :: tonew, True)
               else (Just p, toprev, tonew, False))
      (Nothing, [], [], False)
      phs

separateSamePhones : List Phone -> (List Phone, List Phone)
separateSamePhones phs = separate equalPhones phs

separateUnpronounceable : List Phone -> (List Phone, List Phone)
separateUnpronounceable phs = separate unpronounceablePhones phs

tryNext : (List a, List a) -> (List a -> (List a, List a)) -> (List a, List a)
tryNext (h, r) f =
  if List.isEmpty h
     then f r
     else (h, r)

divideConsonants : List (List Phone) -> List (List Phone)
divideConsonants sylls =
  List.reverse 
  <| (\(prev, rest) ->
      case prev of
        Just p -> p :: rest
        Nothing -> rest)
  <| List.foldl
      (\syll (prev, acc) ->
        case prev of
          Just p ->
            let (toprev, newsyll) = separateSamePhones syll
                                    `tryNext` separateUnpronounceable
            in (Just newsyll, (p ++ toprev) :: acc)
          Nothing -> (Just syll, acc))
      (Nothing, [])
      sylls

{-
mergeLonelyVowels : List (List Phone) -> List (List Phone)
mergeLonelyVowels sylls =
  (\(rest, acc) -> List.take 1 sylls ++ if List.isEmpty rest then acc else rest :: acc)
  <| List.foldr
      (\p (next,acc) -> case (List.take 1 <| List.reverse p, next) of
        ([Vowel _], [Vowel _]) -> ([], (p ++ next) :: acc)
        (_, []) -> (p, acc)
        _ -> (p, next :: acc))
      ([], [])
      (List.drop 1 sylls)
-}

syllabize : String -> List String
syllabize word =
  List.map phonesToString
--  <| mergeLonelyVowels
  <| divideConsonants
  <| divideByVowel
  <| filterHyphens
  <| mapIndivisibles
  <| phones word
