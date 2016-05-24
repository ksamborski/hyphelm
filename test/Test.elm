module Main exposing (main)

import String
import List

import TestData exposing (testData)
import Hyphelm exposing (syllabize)

import ElmTest exposing (..)

tests : Test
tests = 
    suite "WordList tests"
        <| List.map (\(k,v) -> test k (assertEqual v (syllabize k))) testData
        -- [test "chudziutki" (assertEqual ["chu","dziut","ki"] (syllabize "chudziutki"))]

main : Program Never
main = 
    runSuiteHtml tests
