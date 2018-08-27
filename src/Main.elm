module Main exposing (Model, Msg(..), Nodes(..), Step(..), init, main, next, start, traverse, unwind, update, view)

import Browser
import Heap exposing (Heap)
import Html exposing (..)
import Html.Events exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model a =
    Step a


type Step a
    = Compare (Nodes a) (Nodes a) (NodeList a) (List a)
    | Pick (NodeList a) (List a)
    | Sorted (List a)


type Nodes a
    = Leaf a
    | Node a (NodeList a)


type alias NodeList a =
    List (Nodes a)


init : Model Int
init =
    let
        _ =
            [ test [] []
            , test [ 1 ] [ 1 ]
            , test [ 1, 2 ] [ 1, 2 ]
            , test [ 1, 2, 3 ] [ 1, 3, 2 ]
            , test [ 1, 2, 3, 4 ] [ 2, 4, 3, 1 ]
            ]
    in
    [ 3, 1, 2 ]
        |> List.map Leaf
        |> start


test : List Int -> List Int -> Bool
test expected input =
    if
        input
            |> log "test"
            |> List.map Leaf
            |> start
            |> traverse
            |> unwind
            |> (==) expected
    then
        True

    else
        Debug.todo ("got " ++ Debug.toString input ++ " instead of " ++ Debug.toString expected)


log : String -> a -> a
log str input =
    let
        _ =
            Debug.log ("\n" ++ str ++ " ") input
    in
    input


unwind : Step a -> List a
unwind step =
    case step of
        Sorted list ->
            List.reverse list
                |> Debug.log "sorted list"

        _ ->
            Debug.todo "TODO unwind error"


start : NodeList a -> Step a
start list =
    case list of
        [] ->
            Sorted []

        [ a ] ->
            Sorted (nodeToList a)

        a :: b :: rest ->
            Compare a b rest []


nodeToList : Nodes a -> List a
nodeToList node =
    case node of
        Leaf a ->
            [ a ]

        Node a child ->
            a :: nodeListToList child


nodeListToList : NodeList a -> List a
nodeListToList list =
    case list of
        [] ->
            []

        a :: rest ->
            nodeToList a ++ nodeListToList rest


traverse : Step Int -> Step Int
traverse step =
    case Debug.log "traverse" step of
        Pick (one :: two :: rest) sorted ->
            Compare one two rest sorted
                |> traverse

        Pick [ Node current children ] sorted ->
            Pick children (current :: sorted)
                |> traverse

        Pick [ Leaf a ] sorted ->
            Sorted (a :: sorted)

        Pick [] sorted ->
            Sorted sorted

        Compare one two others sorted ->
            Pick (others ++ [ next one two ]) sorted
                |> traverse

        Sorted list ->
            Sorted list


next : Nodes Int -> Nodes Int -> Nodes Int
next one two =
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



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model a -> Model a
update msg model =
    case msg of
        NoOp ->
            model
                |> Debug.log "NoOp"



-- VIEW


view : Model a -> Html Msg
view model =
    h2 [] [ text "todo" ]
