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
        -- [test "abadański" (assertEqual ["a","ba","dań","ski"] (syllabize "abadański"))]

main : Program Never
main = 
    runSuiteHtml tests
