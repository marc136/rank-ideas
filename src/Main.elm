module Main exposing (main)

import Browser
import Heap exposing (Heap)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model a =
    Step a


type Step a
    = Compare (Nodes a) (Nodes a) (NodeList a) (List a)
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



-- TESTS


test : List Int -> List Int -> Bool
test expected input =
    let
        actual =
            input
                |> log "test"
                |> List.map Leaf
                |> start
                |> testTraverseLoop
                |> unwind
    in
    if actual == expected then
        True

    else
        Debug.todo ("got " ++ Debug.toString actual ++ " instead of " ++ Debug.toString expected)


testTraverseLoop : Step Int -> Step Int
testTraverseLoop step =
    case Debug.log "traverseLoop" step of
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
                |> Debug.log "sorted list"

        _ ->
            Debug.todo "TODO unwind error"


log : String -> a -> a
log str input =
    let
        _ =
            Debug.log ("\n" ++ str ++ " ") input
    in
    input


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


pick : NodeList a -> List a -> Step a
pick list sorted =
    let
        _ =
            Debug.log "pick" ( list, sorted )
    in
    case list of
        one :: two :: rest ->
            Compare one two rest sorted

        [ Node current children ] ->
            pick children (current :: sorted)

        [ Leaf a ] ->
            toSorted (a :: sorted)

        [] ->
            toSorted sorted


toSorted : List a -> Step a
toSorted reversedList =
    Sorted <| List.reverse reversedList



-- UPDATE


type Msg
    = NoOp
    | PickFirst
    | PickSecond


update : Msg -> Model a -> Model a
update msg model =
    case msg of
        NoOp ->
            model
                |> Debug.log "NoOp"

        PickFirst ->
            case model of
                Compare a b others sorted ->
                    pick (append a b :: others) sorted

                _ ->
                    Debug.todo "Invalid state"

        PickSecond ->
            case model of
                Compare a b others sorted ->
                    pick (append b a :: others) sorted

                _ ->
                    Debug.todo "Invalid state"


append : Nodes a -> Nodes a -> Nodes a
append parent child =
    case parent of
        Leaf a ->
            Node a [ child ]

        Node a children ->
            Node a (child :: children)



-- VIEW


view : Model a -> Html Msg
view model =
    case Debug.log "model" model of
        Compare one two _ _ ->
            div [ class "pick" ]
                [ h2 [] [ text "Which one should have a higher priority?" ]
                , button [ onClick PickFirst ] [ text <| Debug.toString one ]
                , button [ onClick PickSecond ] [ text <| Debug.toString two ]
                ]

        Sorted list ->
            div [ class "result" ]
                [ h2 [] [ text "Your prioritization" ]
                , ol [] <|
                    List.map item list
                ]


item : a -> Html msg
item a =
    li [] [ text <| Debug.toString a ]
