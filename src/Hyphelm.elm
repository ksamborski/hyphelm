module Hyphelm exposing (syllabize)

import String
import List

type Phone
  = Vowel String
  | Consonant String

mergePhones : Phone -> Phone -> Maybe Phone
mergePhones p1 p2 =
  case (p1, p2) of
    (Vowel v1, Vowel v2) -> Just (Vowel (v1 ++ v2))
    (Consonant c1, Consonant c2) -> Just (Consonant (c1 ++ c2))
    _ -> Nothing

equalPhones : Phone -> Phone -> Bool
equalPhones p1 p2 =
  case (p1, p2) of
    (Vowel v1, Vowel v2) -> (String.toLower v1) == (String.toLower v2)
    (Consonant c1, Consonant c2) -> (String.toLower c1) == (String.toLower c2)
    _ -> False

unpronounceablePhones : Phone -> Phone -> Bool
unpronounceablePhones p1 p2 =
  case (p1, p2) of
    (Consonant c1, Consonant c2) ->
      let (c1', c2') = (String.toLower c1, String.toLower c2)
      in List.member c1' ["ń"]
         || List.member c2' ["ń"]
         || case (c1' ++ c2') of
              "bc" -> True
              "bch" -> True
              "pc" -> True
              "bd" -> True
              "pd" -> True
              "bs" -> True
              "ps" -> True
              "cs" -> True
              "rs" -> True
              "nd" -> True
              "nk" -> True
              "lb" -> True
              "rd" -> True
              "js" -> True
              "tk" -> True
              "kt" -> True
              "nc" -> True
              "jn" -> True
              "nt" -> True
              "tn" -> True
              "ls" -> True
              "bg" -> True
              "czn" -> True
              "wn" -> True
              "ms" -> True
              "ln" -> True
              "jcz" -> True
              "ck" -> True
              "kc" -> True
              "bn" -> True
              "nb" -> True
              "lw" -> True
              "rb" -> True
              "jk" -> True
              "rc" -> True
              "zk" -> True
              "ks" -> True
              "ld" -> True
              "rh" -> True
              "żb" -> True
              "zt" -> True
              "szm" -> True
              "mcz" -> True
              "lf" -> True
              "cht" -> True
              "chm" -> True
              "zd" -> True
              "sf" -> True
              "ct" -> True
              "dw" -> True
              "cp" -> True
              "rk" -> True
              "dk" -> True
              "nz" -> True
              "pt" -> True
              "dm" -> True
              "czk" -> True
              "jsz" -> True
              "wk" -> True
              "nw" -> True
              "gc" -> True
              "rn" -> True
              "wd" -> True
              "rf" -> True
              "rp" -> True
              "km" -> True
              "mp" -> True
              "rch" -> True
              "gf" -> True
              "lk" -> True
              "lh" -> True
              "lg" -> True
              "lm" -> True
              "żk" -> True
              "mk" -> True
              "lc" -> True
              "dg" -> True
              "rm" -> True
              "lch" -> True
              "mb" -> True
              "ndz" -> True
              "dcz" -> True
              "rcz" -> True
              "jz" -> True
              "lt" -> True
              "lz" -> True
              "dzk" -> True
              "ns" -> True
              "szcz" -> True
              "szk" -> True
              "wc" -> True
              "rt" -> True
              "rw" -> True
              "lp" -> True
              "ts" -> True
              "łcz" -> True
              "mr" -> True
              "ng" -> True
              "rg" -> True
              "nh" -> True
              "mf" -> True
              "mh" -> True
              "łk" -> True
              "sb" -> True
              "jd" -> True
              "nż" -> True
              "lcz" -> True
              "szt" -> True
              "rł" -> True
              "rsz" -> True
              "tb" -> True
              "tm" -> True
              "td" -> True
              "gm" -> True
              "rdz" -> True
              "np" -> True
              "lr" -> True
              "fk" -> True
              "jg" -> True
              "szw" -> True
              "mn" -> True
              "bk" -> True
              "gs" -> True
              "tcz" -> True
              _ -> False
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

indivisibleVowels : Phone -> Phone -> Bool
indivisibleVowels v1 v2 =
  case (v1, v2) of
    (Vowel v1', Vowel v2') ->
      List.member
        (String.toLower v1' ++ String.toLower v2')
        ["au", "eu", "ia", "ią", "ie", "ię", "io", "iu", "ió"]
    _ -> False

indivisibleConsonants : Phone -> Phone -> Bool
indivisibleConsonants c1 c2 =
  case (c1, c2) of
    (Consonant c1', Consonant c2') ->
      List.member
        (String.toLower c1' ++ String.toLower c2')
        ["cz", "rz", "sz", "dz", "ch"]
    _ -> False

mapIndivisibles : List Phone -> List Phone
mapIndivisibles phs =
  List.reverse
  <| (\(mp, rest) -> case mp of
       Just p -> p :: rest
       _ -> rest)
  <| List.foldl
    (\ph (prev, acc) ->
      case prev of
        Just p ->
          if indivisibleVowels p ph || indivisibleConsonants p ph
            then case mergePhones p ph of
              Just newp -> (Nothing, newp :: acc)
              Nothing -> (Just ph, p :: acc)
            else (Just ph, p :: acc)
        Nothing -> (Just ph, acc)) 
    (Nothing, [])
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
               else 
                case p of
                  Vowel _ -> (Nothing, toprev, p :: prev :: tonew, True)
                  _ ->
                    if f prev p
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

syllabize : String -> List String
syllabize word =
  List.map phonesToString
  <| divideConsonants
  <| divideByVowel
  <| filterHyphens
  <| mapIndivisibles
  <| phones word
