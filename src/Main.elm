module Main exposing (Nodes(..), Step(..), main, pick, start)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { page : Page
    }


type Page
    = Collect Idea (List Idea)
    | Ranking (Step Idea)


type Idea
    = Title String


type Step a
    = Compare (Nodes a) (Nodes a) (NodeList a) (List a)
    | Sorted (List a)


type Nodes a
    = Leaf a
    | Node a (NodeList a)


type alias NodeList a =
    List (Nodes a)


init : Model
init =
    { page = Collect (Title "") []
    }


log : String -> a -> a
log str input =
    let
        _ =
            Debug.log ("\n" ++ str ++ " ") input
    in
    input


start : List a -> Step a
start list =
    case List.map Leaf list of
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
    -- let _ = Debug.log "pick" ( list, sorted ) in
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
    | AddIdea
    | EditNewTitle String
    | StartRanking
    | PickFirst
    | PickSecond


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        EditNewTitle value ->
            case model.page of
                Collect (Title _) list ->
                    { model | page = Collect (Title value) list }

                _ ->
                    model

        AddIdea ->
            case model.page of
                Collect idea ideas ->
                    { model | page = Collect (Title "") (idea :: ideas) }

                _ ->
                    model

        StartRanking ->
            case model.page of
                Collect idea ideas ->
                    { model | page = Ranking <| start ideas }

                _ ->
                    model

        PickFirst ->
            case model.page of
                Ranking (Compare a b others sorted) ->
                    { model
                        | page =
                            pick (append a b :: others) sorted
                                |> Ranking
                    }

                _ ->
                    model

        PickSecond ->
            case model.page of
                Ranking (Compare a b others sorted) ->
                    { model
                        | page =
                            pick (append b a :: others) sorted
                                |> Ranking
                    }

                _ ->
                    model


append : Nodes a -> Nodes a -> Nodes a
append parent child =
    case parent of
        Leaf a ->
            Node a [ child ]

        Node a children ->
            Node a (child :: children)



-- VIEW


view : Model -> Html Msg
view model =
    case model.page of
        Collect latest ideas ->
            viewCollect latest ideas

        Ranking step ->
            viewRanking step


viewCollect : Idea -> List Idea -> Html Msg
viewCollect latest ideas =
    div [ class "collect" ]
        [ div [ class "new" ] <| collectNewIdea latest
        , ul [ class "ideas" ] <|
            List.map item ideas
        , button [ class "primary", onClick StartRanking ]
            [ text "Start ranking" ]
        ]


collectNewIdea : Idea -> List (Html Msg)
collectNewIdea idea =
    case idea of
        Title title ->
            [ label [ for "new-title" ] [ text "Title" ]
            , input
                [ type_ "text"
                , onInput EditNewTitle
                , id "new-title"
                , value title
                ]
                []
            , button [ class "primary", onClick AddIdea ] [ text "Add" ]
            ]


viewRanking : Step Idea -> Html Msg
viewRanking step =
    case step of
        Compare one two _ _ ->
            div [ class "pick" ]
                [ h2 [] [ text "Which one should have a higher priority?" ]
                , button [ onClick PickFirst ] [ text <| toString one ]
                , button [ onClick PickSecond ] [ text <| toString two ]
                ]

        Sorted list ->
            div [ class "result" ]
                [ h2 [] [ text "Your prioritization" ]
                , ol [] <|
                    List.map item list
                ]


toString : Nodes Idea -> String
toString node =
    case node of
        Leaf (Title title) ->
            title

        Node (Title title) _ ->
            title


item : Idea -> Html msg
item idea =
    case idea of
        Title title ->
            li title


li : String -> Html msg
li caption =
    Html.li [] [ text caption ]
