module TestSteps exposing (sortIntLists)

import Expect exposing (Expectation)
import Fuzz
import Main exposing (Nodes(..), Step(..), pick, start)
import Test exposing (..)


sortIntLists : Test
sortIntLists =
    describe "Sort numbers ascending"
        [ test [] []
        , test [ 1 ] [ 1 ]
        , test [ 1, 2 ] [ 2, 1 ]
        , test [ 1, 2, 3 ] [ 1, 3, 2 ]
        , test [ 1, 2, 3, 4 ] [ 2, 4, 3, 1 ]
        , fuzz (Fuzz.list Fuzz.int) "Sort fuzzed Integer lists" <|
            \input -> check (List.sort input) input
        ]


test : List Int -> List Int -> Test
test expected input =
    Test.test (Debug.toString input) <|
        \_ ->
            check expected input


check : List Int -> List Int -> Expectation
check expected input =
    input
        |> List.map Leaf
        |> start
        |> testTraverseLoop
        |> unwind
        |> Expect.equal expected


testTraverseLoop : Step Int -> Step Int
testTraverseLoop step =
    case step of
        Compare one two others sorted ->
            pick (others ++ [ testChoose one two ]) sorted
                |> testTraverseLoop

        Sorted list ->
            Sorted list


testChoose : Nodes Int -> Nodes Int -> Nodes Int
testChoose one two =
    case ( one, two ) of
        ( Leaf a, Leaf b ) ->
            if a <= b then
                Node a [ two ]

            else
                Node b [ one ]

        ( Leaf a, Node b c ) ->
            if a <= b then
                Node a [ two ]

            else
                Node b (one :: c)

        ( Node a c, Leaf b ) ->
            if a <= b then
                Node a (two :: c)

            else
                Node b [ one ]

        ( Node a c, Node b d ) ->
            if a <= b then
                Node a (two :: c)

            else
                Node b (one :: d)


unwind : Step a -> List a
unwind step =
    case step of
        Sorted list ->
            list

        _ ->
            Debug.todo "TODO unwind error"
