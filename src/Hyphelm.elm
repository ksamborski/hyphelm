module Hyphelm exposing (syllabize)

import String
import List
import List.Extra as List
import Set

import Debug

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
  , "lch", "mb", "ndz", "dc", "dcz", "rcz"
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
  , "kd", "gd", "źl", "df", "jl", "tz"
  , "dzl", "nm", "hl", "tf", "kcz", "rś"
  , "łl", "pn", "śk", "wl", "rl", "ncz"
  , "szn", "łdz", "pk", "ćm", "ść", "ćc"
  , "jż", "żn", "zw", "wski", "wska", "hd"
  , "ński", "jski", "lski", "łm", "kb", "pb"
  , "jm", "rski", "ńska", "jska", "lska", "rska"
  , "mski", "mska", "msko", "bsz", "ccia", "ssia", "ccie"
  , "cciu", "ccio", "ssie", "ssio", "ssiu", "ssię", "ssi"
  , "ccię", "ssią", "ccią", "wni", "wnio", "wnia", "cci"
  , "wnie", "wnię", "wnią", "wniu", "rci", "rcio"
  , "rcie", "rciu", "rcię", "rcią", "rcia", "rdzi", "rció"
  , "rdzia", "rdzie", "rdziu", "rdzio", "rdzią", "rdzię"
  , "hdzia", "hdzie", "hdziu", "hdzio", "hdzią", "hdzię"
  , "chdzia", "chdzie", "chdziu", "chdzio", "chdzią", "chdzię"
  , "ndzi", "ndzia", "ndzie", "ndziu", "ndzią", "ndzio"
  , "ndzię", "nci", "ncio", "ncia", "nciu", "ncie", "ncią", "nció"
  , "ncię", "ksie", "ksi", "ksio", "ksia", "ksiu", "księ"
  , "ksią", "rgia", "rgie", "rgio", "rgiu", "rgią", "rgię"
  , "pni", "pnia", "pnio", "pniu", "pnie", "pnią", "pnię"
  , "nski", "nska", "nsko", "nnia", "nnie", "nnio", "nniu"
  , "nnią", "nnię", "nni", "dski", "dsko", "dska", "dską", "dsku"
  , "dskę", "rni", "rnia", "rnie", "rnio", "rniu", "rnię"
  , "rnią", "lnie", "lnia", "lnią", "lniu", "lnię", "lnio", "lni"
  , "tnie", "tnia", "tnią", "tniu", "tnię", "tnio", "tni"
  , "tsie", "tsia", "tsią", "tsiu", "tsię", "tsio", "tsi"
  , "nsie", "nsia", "nsią", "nsiu", "nsię", "nsio", "nsi"
  , "chni", "chnia", "chnie", "chnio", "chniu", "chnię"
  , "dni", "dnia", "dnie", "dnio", "dniu", "dnię"
  , "żni", "żnia", "żnie", "żnio", "żniu", "żnię"
  , "kci", "kcio", "kcia", "kciu", "kcie", "kcią", "kcię", "kció"
  , "gci", "gcio", "gcia", "gciu", "gcie", "gcią", "gcię", "gció"
  , "jni", "jnio", "jnia", "jniu", "jnie", "jnią", "jnię", "jnió"
  , "źni", "źnio", "źnia", "źniu", "źnie", "źnią", "źnię", "źnió"
  , "łni", "łnio", "łnia", "łniu", "łnie", "łnią", "łnię", "łnió"
  , "czni", "cznio", "cznia", "czniu", "cznie", "cznią", "cznię", "cznió"
  , "żc", "pcz", "rj", "łd", "tsz", "ćs", "bski", "bsko", "bska", "bsku"
  , "pcie", "pciu", "pcię", "pcią", "pcia", "pci", "pció", "pcio"
  ]

unpronounceablePhones : Phone -> Phone -> Bool
unpronounceablePhones p1 p2 =
  case (p1, p2) of
    (Consonant c1, Consonant c2) ->
      let (c1', c2') = (String.toLower c1, String.toLower c2)
      in String.startsWith "ń" c1'
         || String.startsWith "ń" c2'
         || Set.member (c1' ++ c2') unpronounceableMass
    (Consonant c1, Vowel c2) ->
      let (c1', c2') = (String.toLower c1, String.toLower c2)
      in String.startsWith "ń" c1'
         || String.startsWith "ń" c2'
         || Set.member (c1' ++ c2') unpronounceableMass
    (Vowel c1, Consonant c2) ->
      let (c1', c2') = (String.toLower c1, String.toLower c2)
      in String.startsWith "ń" c1'
         || String.startsWith "ń" c2'
         || Set.member (c1' ++ c2') unpronounceableMass
    _ -> False

vowels : Set.Set String
vowels =
  Set.fromList ["a", "ą", "e", "ę", "i", "o", "u", "ó", "y"]

isVowel : Char -> Bool
isVowel s =
  Set.member (String.toLower <| String.fromChar s) vowels

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
  Set.fromList
    [ "au", "eu", "ia", "ią", "ie", "ię", "io"
    , "iu", "ió", "ue", "ay", "ski", "ska", "sko"
    , "dzia", "dzie", "dzią", "dzię", "dziu", "dzio"
    , "dza", "dze", "dzu", "dzy", "dzo", "cie", "cia"
    , "ciu", "cię", "cią", "cio", "ci", "sie", "sia"
    , "siu", "się", "sią", "sio", "si", "nie", "nia"
    , "niu", "nię", "nią", "nio", "ni", "zie", "zia"
    , "ziu", "zię", "zią", "zio", "zi", "gie", "gia"
    , "gią", "gię", "giu", "gio", "rie", "ria", "riu"
    , "rię", "rią", "rio", "oy", "ció", "sió", "nió"
    , "rió"
    ]

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
                  then (mergePhones prev) :: rest
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
          in if indivisibleVowels four || indivisibleConsonants four
              then (List.drop 4 four, mergePhones four :: acc)
              else if indivisibleVowels three || indivisibleConsonants three
                    then (List.drop 3 four, mergePhones three :: acc)
                    else if indivisibleVowels two || indivisibleConsonants two
                          then (List.drop 2 four, mergePhones two :: acc)
                          else (List.drop 1 four, List.take 1 prev ++ acc)

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
               else case p of 
                 Vowel _ -> (Nothing, toprev, p :: tonew, True)
                 _ -> (Just p, toprev, tonew, False))
               
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

mergeLonelyVowels : List (List Phone) -> List (List Phone)
mergeLonelyVowels sylls =
  case List.head sylls of
    Just fstSyll ->
      fstSyll ::
      (List.reverse <| snd <| List.foldl
         (\mthree (merged, acc) ->
           case mthree of
             [] -> (False, acc)
             [p1] -> (False, acc)
             [p1,p2] -> if merged
                          then (False, acc)
                          else (False, p2 :: acc)
             [p1,[Vowel v2],[Vowel v3]] ->
               (False, [Vowel v2] :: acc)
             [[Vowel v1],[Vowel v2],p3] ->
               (False, [Vowel v2] :: acc)
             [p1,[Vowel v2],p3] ->
               if String.any (\c -> not <| Set.member (String.fromChar c) vowels) v2
                 then (False, [Vowel v2] :: acc)
                 else case List.last p1 of
                   Just (Vowel v1) ->  (True, (Vowel v2 :: p3) :: acc)
                   _ -> (False, [Vowel v2] :: acc)
             [p1,p2,p3] -> if merged
                            then (False, acc)
                            else (False, p2 :: acc)
             _ -> (False, acc))
       (False, [])
       <| List.greedyGroupsOfWithStep 3 1 sylls)
    Nothing -> sylls

mergeLonelyConsonants : List (List Phone) -> List (List Phone)
mergeLonelyConsonants sylls =
  let len = List.length sylls
  in if len > 1
      then let (i,r) = List.splitAt (len - 2) sylls
           in if List.all phoneIsC <| List.concat <| List.drop 1 r
               then i ++ [List.concat r]
               else sylls
      else sylls

syllabize : String -> List String
syllabize word =
  List.map phonesToString
  <| mergeLonelyConsonants
  <| mergeLonelyVowels
  <| divideConsonants
  <| divideByVowel
  <| filterHyphens
  <| mapIndivisibles
  <| phones word
